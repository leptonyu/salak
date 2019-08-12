{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoOverloadedLists          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
-- Module:      Salak.Internal
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module is used for implementing loaders.
--
module Salak.Internal(
    loadAndRunSalak'
  , loadTrie
  , loadList
  , LoadSalakT
  , LoadSalak
  , RunSalakT
  , RunSalak
  , runRun
  , MonadSalak(..)
  , loadMock
  , loadEnv
  , loadCommandLine
  , ParseCommandLine
  , defaultParseCommandLine
  , tryLoadFile
  , Source
  , TraceSource
  , Keys(..)
  , Key(..)
  , simpleKeys
  , ToKeys(..)
  , setVal
  , Val(..)
  , Value(..)
  , ToValue(..)
  , liftNT
  , SourcePack(..)
  , MonadIO
  , module Salak.Internal.Writable
  ) where


import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Control.Monad.IO.Unlift as IU
import           Control.Monad.Reader
import qualified Control.Monad.State     as MS
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import           Data.Text               (Text, pack)
import qualified Data.Text               as TT
import           Salak.Internal.Key
import           Salak.Internal.Prop
import           Salak.Internal.Source
import           Salak.Internal.Val
import           Salak.Internal.Writable
import qualified Salak.Trie              as T
import           System.Directory
import           System.Environment

data UpdateSource = UpdateSource
  {  ref    :: !(MVar Source)
  ,  refNo  :: !Int
  ,  refMap :: !(HashMap Int String)
  ,  lfunc  :: !(MVar LFunc)
  ,  qfunc  :: !(MVar QFunc)
  ,  update :: !(MVar (IO ( TraceSource -- Updated Tries
                  , IO ())))    -- Confirm action
  }

-- | Configuration Loader Monad, used for load properties from sources. Custom loaders using `loadTrie`
newtype LoadSalakT  m a = LoadSalakT (MS.StateT UpdateSource m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MS.MonadState UpdateSource, MonadThrow, MonadCatch)

-- | Simple IO Monad
type LoadSalak = LoadSalakT IO

runLoad :: Monad m => LoadSalakT m a -> UpdateSource -> m a
runLoad (LoadSalakT ma) = MS.evalStateT ma

liftNT :: MonadIO m => LoadSalak () -> LoadSalakT m ()
liftNT a = MS.get >>= liftIO . runLoad a

instance MonadIO m => MonadReader SourcePack (LoadSalakT m) where
  ask = MS.get >>= toSourcePack
  local _ ma = ma

instance (MonadThrow m, IU.MonadUnliftIO m) => IU.MonadUnliftIO (LoadSalakT m) where
  askUnliftIO = do
    ut <- MS.get
    lift $ IU.withUnliftIO $ \u -> return (IU.UnliftIO (IU.unliftIO u . flip runLoad ut))

-- | Standard `MonadSalak` instance.
newtype RunSalakT m a = RunSalakT (ReaderT SourcePack m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader SourcePack, MonadThrow, MonadCatch)

-- | Simple IO Monad
type RunSalak = RunSalakT IO

runRun :: Monad m => RunSalakT m a -> SourcePack -> m a
runRun (RunSalakT ma) = runReaderT ma

instance (MonadThrow m, IU.MonadUnliftIO m) => IU.MonadUnliftIO (RunSalakT m) where
  askUnliftIO = do
    ut <- ask
    lift $ IU.withUnliftIO $ \u -> return (IU.UnliftIO (IU.unliftIO u . flip runRun ut))

-- | Basic loader
loadTrie :: (MonadThrow m, MonadIO m) => Bool -> String -> (Int -> IO TraceSource) -> LoadSalakT m ()
loadTrie canReload name f = do
  logSalak $ "Loading " ++ (if canReload then "[reloadable]" else "") ++ name
  UpdateSource{..} <- MS.get
  v              <- liftIO $ readMVar ref
  ts             <- liftIO $ loadSource f refNo (fmap ([],) v)
  let (t,_,es) = extract v ts
  if null es
    then do
      liftIO $ modifyMVar_ update $ \u -> go ts u refNo
      let nut = UpdateSource ref (refNo + 1) (HM.insert refNo name refMap) lfunc qfunc update
      _ <- liftIO $ swapMVar ref t
      MS.put nut
    else throwM $ PropException $ unlines es
  where
    go ts ud n = return $ do
      (c,d) <- ud
      c1    <- loadSource (if canReload then f else (\_ -> return ts)) n c
      return (c1,d)

-- | Simple loader
loadList :: (MonadThrow m, MonadIO m, Foldable f, ToKeys k, ToValue v) => Bool -> String -> IO (f (k,v)) -> LoadSalakT m ()
loadList canReload name iof = loadTrie canReload name (\i -> gen i <$> iof)

-- | Standard salak functions, by load and with a `SourcePack` instance.
--  Users should use `SourcePack` to create custom `MonadSalak` instances, then you get will an instance of `MonadSalak`.
loadAndRunSalak' :: (MonadThrow m, MonadIO m) => LoadSalakT m () -> (SourcePack -> m a) -> m a
loadAndRunSalak' lstm f = load lstm >>= f

load :: (MonadThrow m, MonadIO m) => LoadSalakT m () -> m SourcePack
load lm = do
  r <- liftIO $ newMVar T.empty
  q <- liftIO $ newMVar $ \s -> Right $ void $ swapMVar r s
  u <- liftIO $ newMVar $ return (T.empty, return ())
  l <- liftIO $ newMVar $ \_ -> return ()
  runLoad (lm >> MS.get) (UpdateSource r 0 HM.empty l q u) >>= toSourcePack

toSourcePack :: MonadIO m => UpdateSource -> m SourcePack
toSourcePack UpdateSource{..} = liftIO (readMVar ref) >>= \s -> return $ SourcePack s [] qfunc lfunc go
  where
    go = do
      t        <- readMVar ref
      (ts, ac) <- join $ readMVar update
      let (s,cs,es) = extract t ts
      f <- readMVar qfunc
      if null es
        then case f s of
          Left  e -> return (ReloadResult True $ lines e)
          Right a -> ac >> a >> return (ReloadResult False $ lines $ show cs)
        else return (ReloadResult True es)

-- | Load mock variables into `Source`
loadMock :: (MonadThrow m, MonadIO m) => [(Text, Text)] -> LoadSalakT m ()
loadMock fa = loadList False "mock" (return fa)

-- | Load environment variables into `Source`
loadEnv :: (MonadThrow m, MonadIO m) => LoadSalakT m ()
loadEnv = loadList False "environment" go
  where
    go = concatMap split2 . filter ((/= '_') . head . fst) <$> getEnvironment
    split2 (k,v) = [(TT.pack k,v),(convert k,v)]
    convert = TT.toLower . TT.pack . map (\c -> if c == '_' then '.' else c)

-- | Convert arguments to properties
type ParseCommandLine = [String] -> IO [(Text, Text)]

-- | Default way to parse command line arguments
defaultParseCommandLine :: ParseCommandLine
defaultParseCommandLine = return . mapMaybe go
  where
    go ('-':'-':as) = case break (=='=') as of
      (a,'=':b) -> Just (pack a, pack b)
      _         -> Nothing
    go _ = Nothing

-- | Default way to parse command line arguments
loadCommandLine :: (MonadThrow m, MonadIO m) => ParseCommandLine -> LoadSalakT m ()
loadCommandLine pcl = loadList False "commandLine" (getArgs >>= pcl)

-- | Try load file, if file does not exist then do nothing.
tryLoadFile :: MonadIO m => (FilePath -> LoadSalakT m ()) -> FilePath -> LoadSalakT m ()
tryLoadFile f file = do
  b <- liftIO $ doesFileExist file
  if b
    then f file
    else logSalak $ "File does not exist, ignore load " ++ file

