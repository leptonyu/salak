{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module Salak.Load.Dynamic where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.IO.Unlift
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.IntMap.Strict      as MI
import           Data.Text               (Text)
import           Salak.Prop
import           Salak.Types
import           Salak.Types.Source

data ReloadResult = ReloadResult
  { isError :: Bool     -- ^ msg stands for properties changing record if true, otherwise msg means reload error.
  , msg     :: [String] -- ^ message log
  } deriving (Eq, Show)

-- | Reloadable SourcePack
data ReloadableSourcePack = ReloadableSourcePack
  { sourcePack :: MVar SourcePack
  , logs       :: [Text]
  , reloadAll  :: (SourcePack -> IO ([IO ()], [String])) -> IO ReloadResult
  }

reloadableSourcePack :: MonadIO m => SourcePack -> m ReloadableSourcePack
reloadableSourcePack sp = do
  msp <- liftIO $ newMVar sp
  return $ ReloadableSourcePack msp [] (reloadAll' msp)
  where
    reloadAll' v f = do
      sp' <- readMVar v
      as  <- sequence $ MI.foldlWithKey' runReload [] (reEnv sp')
      let loadErr = concat $ fst . snd <$> as
          runWith e a = if null e then a else return $ ReloadResult True e
      runWith loadErr $ do
        let sp''    = foldl g2 sp' {errs = []} as
            modLog  = errs sp''
        (ac, msErr) <- f sp''
        runWith msErr $ swapMVar v sp'' >> sequence_ ac >> return (ReloadResult False modLog)
    g2 :: SourcePack -> (Int, ([String], Source)) -> SourcePack
    g2 p (i, (_, s)) = let (s', e) = runWriter $ replace i s (source p) in p { source = s', errs = errs p <> e}

-- | RunSalak Monad Transfer
newtype RunSalakT m a = RunSalakT { unRun :: StateT ReloadableSourcePack m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance MonadIO m => MonadIO (RunSalakT m) where
  liftIO = lift . liftIO

instance MonadUnliftIO m => MonadUnliftIO (RunSalakT m) where
  askUnliftIO = RunSalakT $ do
    rsp <- get
    f   <- lift askUnliftIO
    return $ UnliftIO $ unliftIO f . (`evalStateT` rsp) . unRun

liftNT :: Monad n => (forall x. m x -> n x) -> RunSalakT m a -> RunSalakT n a
liftNT f (RunSalakT ma) = RunSalakT $ do
  rsp    <- get
  (a, s) <- lift $ f (runStateT ma rsp)
  put s
  return a

askRSP :: MonadIO m => RunSalakT m SourcePack
askRSP = RunSalakT $ do
  ReloadableSourcePack{..} <- get
  liftIO $ readMVar sourcePack

search' :: (MonadIO m, FromProp a) => Text -> RunSalakT m (Either String (IO a))
search' k = RunSalakT $ do
  sp <- unRun askRSP
  case search k sp of
    Left  e -> return (Left e)
    Right r -> do
      v  <- liftIO $ newMVar r
      modify $ \rsp -> rsp { reloadAll = reloadAll rsp . go v}
      return $ Right $ readMVar v
  where
    go x f sp = do
      (as,es) <- f sp
      case search k sp of
        Left  e -> return (as, e:es)
        Right r -> return (void (swapMVar x r):as, es)

reloadAction :: Monad m => RunSalakT m (IO ReloadResult)
reloadAction = RunSalakT $ do
  ReloadableSourcePack{..} <- get
  return $ reloadAll $ \_ -> return ([], [])

runT :: MonadIO m => RunSalakT m a -> SourcePack -> m a
runT (RunSalakT a) sp = reloadableSourcePack sp >>= evalStateT a

-- | Run action in `RunSalakT`, `IO` `ReloadResult` is reloadable action.
exec :: MonadIO m => (IO ReloadResult -> IO a) -> RunSalakT m a
exec fa = reloadAction >>= lift . liftIO . fa
