{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Salak.Load.Dynamic where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.IntMap.Strict      as MI
import           Data.Text               (Text)
import           Salak.Prop
import           Salak.Types
import           Salak.Types.Source

data ReloadResult = ReloadResult
  { isError :: Bool
  , msg     :: [String]
  } deriving (Eq, Show)

-- | Reloadable SourcePack
data ReloadableSourcePack = ReloadableSourcePack
  { sourcePack :: MVar SourcePack
  , reloadAll  :: (SourcePack -> IO ([IO ()], [String])) -> IO ReloadResult
  }

reloadableSourcePack :: MonadIO m => SourcePack -> m ReloadableSourcePack
reloadableSourcePack sp = do
  msp <- liftIO $ newMVar sp
  return $ ReloadableSourcePack msp (reloadAll' msp)
  where
    reloadAll' v f = do
      sp' <- readMVar v
      as  <- sequence $ MI.foldlWithKey' go [] (reEnv sp')
      let loadErr = concat $ fst . snd <$> as
          runWith e a = if null e then a else return $ ReloadResult True e
      runWith loadErr $ do
        let sp''    = foldl g2 sp' {errs = []} as
            modLog  = errs sp''
        (ac, msErr) <- f sp''
        runWith msErr $ putMVar v sp'' >>  sequence_ ac >> return (ReloadResult False modLog)
    go b i (Reload _ f) = ((i,) <$> f i) : b
    g2 :: SourcePack -> (Int, ([String], Source)) -> SourcePack
    g2 p (i, (_, s)) = let (s', e) = runWriter $ replace i s (source p) in p { source = s', errs = errs p <> e}

type ReloadableSourcePackT = StateT ReloadableSourcePack

search' :: (MonadIO m, FromProp a) => Text -> ReloadableSourcePackT m (Either String (IO a))
search' k = do
  ReloadableSourcePack{..}  <- get
  sp <- liftIO $ takeMVar sourcePack
  case search k sp of
    Left  e -> return (Left e)
    Right r -> do
      v  <- liftIO $ newMVar r
      put (ReloadableSourcePack sourcePack (reloadAll . go v))
      return $ Right $ readMVar v
  where
    go x f sp = do
      (as,es) <- f sp
      case search k sp of
        Left  e -> return (as, e:es)
        Right r -> return (putMVar x r:as, es)

reloadAction :: Monad m => ReloadableSourcePackT m (IO ReloadResult)
reloadAction = do
  ReloadableSourcePack{..} <- get
  return $ reloadAll $ \_ -> return ([], [])

runReloadable :: MonadIO m => ReloadableSourcePackT m a -> SourcePack -> m a
runReloadable r sp = reloadableSourcePack sp >>= evalStateT r



