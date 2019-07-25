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
  , runRunSalak
  , HasSalak(..)
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
  ) where


import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch
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
import qualified Salak.Trie              as T
import           System.Directory
import           System.Environment

data UpdateSource = UpdateSource
  {  ref    :: MVar Source
  ,  refNo  :: Int
  ,  refMap :: HashMap Int String
  ,  qfunc  :: MVar QFunc
  ,  update :: MVar (IO ( TraceSource -- Updated Tries
                  , IO ()))    -- Confirm action
  }

-- | Configuration Loader Monad, used for load properties from sources. Custom loaders using `loadTrie`
newtype LoadSalakT  m a = LoadSalakT  { unLoad  :: MS.StateT UpdateSource m a } deriving (Functor, Applicative, Monad, MS.MonadTrans)

-- | Simple IO Monad
type LoadSalak = LoadSalakT IO

liftNT :: MonadIO m => LoadSalak () -> LoadSalakT m ()
liftNT (LoadSalakT a) = do
  ud <- MS.get
  MS.liftIO $ MS.evalStateT a ud

instance MonadIO m => MonadSalak (LoadSalakT m) where
  askSalak = MS.get >>= toSourcePack

instance MonadThrow m => MonadThrow (LoadSalakT m) where
  throwM = LoadSalakT . throwM

instance MonadCatch m => MonadCatch (LoadSalakT m) where
  catch m f = do
    us <- MS.get
    lift $ MS.evalStateT (unLoad m) us `catch` (\e -> MS.evalStateT (unLoad $ f e) us)

instance Monad m => MS.MonadState UpdateSource (LoadSalakT m) where
  state f = LoadSalakT $ MS.state f

instance MonadIO m => MonadIO (LoadSalakT m) where
  liftIO = LoadSalakT . liftIO

instance IU.MonadUnliftIO m => IU.MonadUnliftIO (LoadSalakT m) where
  askUnliftIO = LoadSalakT $ do
    ut <- MS.get
    f  <- MS.lift IU.askUnliftIO
    return $ IU.UnliftIO $ IU.unliftIO f . (`MS.evalStateT` ut) . unLoad

-- | Standard `HasSalak` instance.
newtype RunSalakT m a = RunSalakT { unRun :: ReaderT SourcePack m a } deriving (Functor, Applicative, Monad, MonadTrans)

-- | Simple IO Monad
type RunSalak = RunSalakT IO

instance Monad m => MonadSalak (RunSalakT m) where
  askSalak = RunSalakT ask

instance Monad m => MonadReader SourcePack (RunSalakT m)  where
  ask = RunSalakT ask
  local f m = RunSalakT $ local f $ unRun m

instance MonadThrow m => MonadThrow (RunSalakT m) where
  throwM = RunSalakT . throwM

instance MonadCatch m => MonadCatch (RunSalakT m) where
  catch m f = do
    us <- ask
    lift $ runReaderT (unRun m) us `catch` (\e -> runReaderT (unRun $ f e) us)

instance MonadIO m => MonadIO (RunSalakT m) where
  liftIO = RunSalakT . liftIO

instance IU.MonadUnliftIO m => IU.MonadUnliftIO (RunSalakT m) where
  askUnliftIO = RunSalakT $ do
    ut <- ask
    f  <- lift IU.askUnliftIO
    return $ IU.UnliftIO $ IU.unliftIO f . (`runReaderT` ut) . unRun

-- | Automatic promote @t@ (`RunSalakT` @m@) into `MonadSalak` instance.
instance {-# OVERLAPPABLE #-} (m' ~ t (RunSalakT m), MonadTrans t, Monad m, Monad m') => MonadSalak m' where
  askSalak = lift askSalak

runRunSalak :: SourcePack -> RunSalakT m a -> m a
runRunSalak sp (RunSalakT m) = runReaderT m sp

-- | Basic loader
loadTrie :: MonadIO m => Bool -> String -> (Int -> IO TraceSource) -> LoadSalakT m ()
loadTrie canReload name f = do
  UpdateSource{..} <- MS.get
  v              <- liftIO $ readMVar ref
  ts             <- liftIO $ loadSource f refNo (fmap ([],) v)
  let (t,_,es) = extract v ts
  if null es
    then do
      liftIO $ modifyMVar_ update $ \u -> go ts u refNo
      let nut = UpdateSource ref (refNo + 1) (HM.insert refNo name refMap) qfunc update
      _ <- liftIO $ swapMVar ref t
      MS.put nut
    else error $ show es
  where
    go ts ud n = return $ do
      (c,d) <- ud
      c1    <- loadSource (if canReload then f else (\_ -> return ts)) n c
      return (c1,d)

-- | Simple loader
loadList :: (MonadIO m, Foldable f, ToKeys k, ToValue v) => Bool -> String -> IO (f (k,v)) -> LoadSalakT m ()
loadList canReload name iof = loadTrie canReload name (\i -> gen i <$> iof)

-- | Standard salak functions, by load and with a `SourcePack` instance.
--  Users should use `SourcePack` to create custom `MonadSalak` instances, then you get will an instance of `HasSalak`.
loadAndRunSalak' :: (MonadThrow m, MonadIO m) => LoadSalakT m () -> (SourcePack -> m a) -> m a
loadAndRunSalak' lstm f = load lstm >>= f

load :: MonadIO m => LoadSalakT m () -> m SourcePack
load (LoadSalakT lm) = do
  r <- liftIO $ newMVar T.empty
  q <- liftIO $ newMVar $ \s -> Right $ void $ swapMVar r s
  u <- liftIO $ newMVar $ return (T.empty, return ())
  MS.execStateT lm (UpdateSource r 0 HM.empty q u) >>= toSourcePack

toSourcePack :: MonadIO m => UpdateSource -> m SourcePack
toSourcePack UpdateSource{..} = liftIO (readMVar ref) >>= \s -> return $ SourcePack s [] qfunc go
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
loadMock :: MonadIO m => [(Text, Text)] -> LoadSalakT m ()
loadMock fa = loadList False "mock" (return fa)

-- | Load environment variables into `Source`
loadEnv :: MonadIO m => LoadSalakT m ()
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
loadCommandLine :: MonadIO m => ParseCommandLine -> LoadSalakT m ()
loadCommandLine pcl = loadList False "commandLine" (getArgs >>= pcl)

-- | Try load file, if file does not exist then do nothing.
tryLoadFile :: MonadIO m => (FilePath -> LoadSalakT m ()) -> FilePath -> LoadSalakT m ()
tryLoadFile f file = do
  b <- liftIO $ doesFileExist file
  when b $ do
    liftIO $ putStrLn $ "Load " ++ file
    f file

