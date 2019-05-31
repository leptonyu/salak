{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module Salak.Types(
    SourcePack(..)
  , emptySourcePack
  , mapSource
  , select
  , addErr
  , tryLoadFile
  , load
  , loadOnce
  , loadMock
  , loadOnceMock
  , runLoadT
  , LoadSalakT(..)
  , jump
  , runReload
  ) where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.IntMap.Strict   as MI
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Salak.Types.Selector
import           Salak.Types.Source
import           Salak.Types.Value
import           System.Directory

data Reload = Reload
  { sourceName :: Text
  , canReload  :: Bool
  , reloadS    :: Priority -> IO ([String], Source)
  }

runReload :: [IO (Priority, ([String], Source))] -> Priority -> Reload -> [IO (Priority, ([String], Source))]
runReload b i Reload{..} = if canReload then ((i,) <$> reloadS i) : b else b

instance Show Reload where
  show (Reload s _ _) = T.unpack s

defReload :: Bool -> String -> LoadSalakT IO () -> Reload
defReload cr s spt = Reload (T.pack s) cr (\i -> go <$> runLoadT (Just i) spt)
  where
    go SourcePack{..} = (errs, source)

emptyReload :: String -> Reload
emptyReload s = defReload False s (return ())

-- | Source package, used to store all properties.
data SourcePack = SourcePack
  { prefix :: [Selector]
  , packId :: Int
  , source :: Source
  , reEnv  :: MI.IntMap Reload
  , errs   :: [String]
  } deriving Show

emptySourcePack :: SourcePack
emptySourcePack = SourcePack [] 0 emptySource mempty []

mapSource :: (Source -> Source) -> SourcePack -> SourcePack
mapSource f sp = sp { source = f (source sp)}

select :: SourcePack -> Selector -> SourcePack
select sp n = sp { source = selectSource n (source sp), prefix = n : prefix sp}

addErr :: Monad m => String -> LoadSalakT m ()
addErr e = LoadSalakT $ get >>= (\sp -> return sp {errs = e : errs sp}) >>= put

loadInternal
  :: Monad m
  => Reload
  -> (Priority -> Source -> WriterT [String] m Source)
  -> LoadSalakT m ()
loadInternal file go = LoadSalakT $ do
  SourcePack{..} <- get
  (s', e) <- lift $ runWriterT $ go packId source
  put $ SourcePack prefix (packId+1) s' (MI.insert packId file reEnv) (errs ++ e)

-- ^ Load properties, supports reload when triggered.
load
  :: MonadIO m
  => String -- ^ Loading name
  -> (Priority -> Source -> WriterT [String] IO Source) -- ^ Convert properties
  -> LoadSalakT m ()
load file go = loadInternal (defReload True file $ load file go) (\i -> x . go i)
  where
    x a = do
      (s, w) <- liftIO $ runWriterT a
      tell w
      return s

tryLoadFile :: MonadIO m => (FilePath -> LoadSalakT m ()) -> FilePath -> LoadSalakT m ()
tryLoadFile f file = do
  b <- liftIO $ doesFileExist file
  when b $ do
    liftIO $ putStrLn $ "Load " <> file
    f file

-- | Load properties only once
loadOnce
  :: (Foldable f, Monad m)
  => String -- ^ Loading name
  -> f a -- ^ Properties
  -> (Priority -> a -> m (Text, Value)) -- ^ Convert properties to Value
  -> LoadSalakT m ()
loadOnce name fa f = loadInternal (emptyReload name) $ \i s -> foldM (go i) s fa
  where
    go i s a = do
      (k, v) <- lift $ f i a
      insert k v s

-- | Put key value pairs into `SourcePack`
loadOnceMock :: Monad m => [(Text, Text)] -> LoadSalakT m ()
loadOnceMock fs = loadOnce "Mock" fs (\i (k,v) -> return (k, newVStr v i))

loadMock :: MonadIO m => [(Text, IO Text)] -> LoadSalakT m ()
loadMock fs = load "Mock" $ \i s -> foldM (go i) s fs
  where
    go :: Priority -> Source -> (Text, IO Text) -> WriterT [String] IO Source
    go i s (k, iov) = do
      v <- lift iov
      insert k (VStr i v) s

runLoadT :: Monad m => Maybe Priority -> LoadSalakT m a -> m SourcePack
runLoadT i (LoadSalakT ac) = execStateT ac emptySourcePack { packId = fromMaybe 0 i }

-- | Load Salak Monad Transfer
newtype LoadSalakT m a = LoadSalakT { unLoad :: StateT SourcePack m a } deriving (Functor, Applicative, Monad, MonadTrans)

instance MonadIO m => MonadIO (LoadSalakT m) where
  liftIO = lift . liftIO

jump :: MonadIO m => LoadSalakT IO a -> LoadSalakT m a
jump (LoadSalakT a) = LoadSalakT $ do
  (a', sp) <- get >>= liftIO . runStateT a
  put sp
  return a'



