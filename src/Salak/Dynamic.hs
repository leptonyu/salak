{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Salak.Dynamic where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.State
import qualified Data.IntMap.Strict      as MI
import           Data.Text               (Text)
import           Salak.Prop
import           Salak.Types

data ReloadableSourcePack = ReloadableSourcePack
  { sourcePack :: MVar SourcePack
  , reloadAll  :: (SourcePack -> IO ([IO ()], [String])) -> IO (Bool, [String])
  }

reloadableSourcePack :: MonadIO m => SourcePack -> m ReloadableSourcePack
reloadableSourcePack sp = do
  msp <- liftIO $ newMVar sp
  return $ ReloadableSourcePack msp (reloadAll' msp)
  where
    reloadAll' v f = do
      sp'@(SourcePack _ _ _ it) <- readMVar v
      as <- filter (not . nullSource . snd) <$> mapM go (MI.toList it)
      if null as
        then return (True, [])
        else do
          let (es, sp'') = extractErr' $ foldl g2 sp' as
          (ac, es') <- f sp''
          if null es'
            then putMVar v sp'' >>  sequence_ ac >> return (True, es)
            else return (False, es')
    go (i, Reload _ f) = (i,) <$> f i
    g2 (SourcePack ss i s it) (x,s') = SourcePack ss i (replace x s' s) it

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

runReloadable :: MonadIO m => SourcePack -> ReloadableSourcePackT m a -> m (a, IO (Bool, [String]))
runReloadable sp r = do
  rsp <- reloadableSourcePack sp
  (a, ReloadableSourcePack{..}) <- runStateT r rsp
  return (a, reloadAll (\_ -> return ([],[])))



