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
module Salak.Internal(
    load
  , loadTrie
  , loadList
  , LoadSalak
  , LoadSalakT
  , RunSalak
  , RunSalakT
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
  ,  update :: IO ( TraceSource -- Updated Tries
                  , IO ())    -- Confirm action
  }

newtype LoadSalakT  m a = LoadSalakT  { unLoad  :: MS.StateT UpdateSource m a } deriving (Functor, Applicative, Monad, MS.MonadTrans)

type LoadSalak = LoadSalakT IO

liftNT :: MonadIO m => LoadSalak () -> LoadSalakT m ()
liftNT (LoadSalakT a) = do
  ud <- MS.get
  MS.liftIO $ MS.evalStateT a ud

instance MonadIO m => MonadSalak (LoadSalakT m) where
  askSalak = MS.get >>= toSourcePack

instance MonadThrow m => MonadThrow (LoadSalakT m) where
  throwM = LoadSalakT . throwM

instance Monad m => MS.MonadState UpdateSource (LoadSalakT m) where
  state f = LoadSalakT $ MS.state f

instance MonadIO m => MonadIO (LoadSalakT m) where
  liftIO = LoadSalakT . liftIO

instance IU.MonadUnliftIO m => IU.MonadUnliftIO (LoadSalakT m) where
  askUnliftIO = LoadSalakT $ do
    ut <- MS.get
    f  <- MS.lift IU.askUnliftIO
    return $ IU.UnliftIO $ IU.unliftIO f . (`MS.evalStateT` ut) . unLoad

newtype RunSalakT m a = RunSalakT { unRun :: ReaderT SourcePack m a } deriving (Functor, Applicative, Monad, MonadTrans)

type RunSalak = RunSalakT IO

instance MonadIO m => MonadSalak (RunSalakT m) where
  askSalak = RunSalakT ask

instance Monad m => MonadReader SourcePack (RunSalakT m)  where
  ask = RunSalakT ask
  local f m = RunSalakT $ local f $ unRun m

instance MonadThrow m => MonadThrow (RunSalakT m) where
  throwM = RunSalakT . throwM

runRunSalak :: SourcePack -> RunSalakT m a -> m a
runRunSalak sp (RunSalakT m) = runReaderT m sp

loadTrie :: MonadIO m => Bool -> String -> (Int -> IO TraceSource) -> LoadSalakT m ()
loadTrie canReload name f = do
  UpdateSource{..} <- MS.get
  v              <- liftIO $ readMVar ref
  ts             <- liftIO $ loadSource f refNo (fmap ([],) v)
  let (t,_,es) = extract v ts
  if null es
    then do
      let nut = UpdateSource ref (refNo + 1) (HM.insert refNo name refMap) qfunc (go update refNo)
      _ <- liftIO $ swapMVar ref t
      MS.put nut
    else error $ show es
  where
    go ud n = if canReload
      then do
        (c,d) <- ud
        c1    <- loadSource f n c
        return (c1,d)
      else ud

loadList :: (MonadIO m, Foldable f, ToKeys k, ToValue v) => Bool -> String -> IO (f (k,v)) -> LoadSalakT m ()
loadList canReload name iof = loadTrie canReload name go
  where
    go i = generate i <$> iof

load :: MonadIO m => LoadSalakT m () -> m SourcePack
load (LoadSalakT lm) = do
  r <- liftIO $ newMVar T.empty
  q <- liftIO $ newMVar $ \_ -> Right (return ())
  let u = return (T.empty, return ())
  MS.execStateT lm (UpdateSource r 0 HM.empty q u) >>= toSourcePack

toSourcePack :: MonadIO m => UpdateSource -> m SourcePack
toSourcePack UpdateSource{..} = liftIO (readMVar ref) >>= \s -> return $ SourcePack s [] qfunc go
  where
    go = do
      t        <- readMVar ref
      (ts, ac) <- update
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

