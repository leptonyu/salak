module Data.Salak.Dynamic(
    LoaderT
  , load
  , runLoader
  , askSetProperties
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State
import           Control.Monad.STM
import qualified Data.HashMap.Strict         as M
import           Data.Salak.Operation
import           Data.Salak.Types
import           Data.Text                   (Text)

type Loader = (IO Properties, Properties -> IO ())

type LoaderT = StateT Loader

dynamicLoader :: Properties -> IO Loader
dynamicLoader mp = do
  m <- newTVarIO mp
  return (readTVarIO m,\np -> atomically $ modifyTVar' m (go np))
  where
    go (Properties [] m1) (Properties p m2) = Properties p (gm m1 m2)
    go (Properties p  m1) (Properties _ m2) = Properties p (gm m1 m2)
    gm [] m    = m
    gm [m] [n] = [M.unionWithKey (\_ -> go) m n]
    gm m _     = m

-- | Load default properties
load :: (FromProperties a, MonadIO m, Show a) => Text -> LoaderT m (IO a)
load key = do
  (iop, ios) <- get
  p <- liftIO iop
  v <- liftIO $ newTVarIO $ p .>> key
  let go np = do
        ios np
        p' <- iop
        let a = p' .>> key
        atomically $ writeTVar v a
  put (iop, go)
  return (readTVarIO v)

-- | Run loader
runLoader :: MonadIO m => Properties -> LoaderT m a -> m a
runLoader p a = do
  l <- liftIO $ dynamicLoader p
  fst <$> runStateT a l

-- | Ask properties setter.
askSetProperties :: MonadIO m => LoaderT m (Properties -> m ())
askSetProperties = do
  (_, ios) <- get
  return $ liftIO . ios
