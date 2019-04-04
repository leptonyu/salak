{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Salak.Types where

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
  , reload     :: Priority -> IO ([String], Source)
  }

instance Show Reload where
  show (Reload s _) = T.unpack s

defReload :: String -> LoadSalakT IO () -> Reload
defReload s spt = Reload (T.pack s) (\i -> go <$> runLoadT (Just i) spt)
  where
    go SourcePack{..} = (errs, source)

emptyReload s = defReload s (return ())

-- | Source package, used to store all properties.
data SourcePack = SourcePack
  { prefix :: [Selector]
  , packId :: Int
  , source :: Source
  , reEnv  :: MI.IntMap Reload
  , errs   :: [String]
  } deriving Show

emptySourcePack = SourcePack [] 0 emptySource mempty []

mapSource :: (Source -> Source) -> SourcePack -> SourcePack
mapSource f sp = sp { source = f (source sp)}

select :: SourcePack -> Selector -> SourcePack
select sp n = sp { source = selectSource n (source sp), prefix = n : prefix sp}

addErr :: Monad m => String -> LoadSalakT m ()
addErr e = LoadSalakT $ get >>= (\sp -> return sp {errs = e : errs sp}) >>= put

loadFile
  :: Monad m
  => Reload
  -> (Priority -> Source -> WriterT [String] m Source)
  -> LoadSalakT m ()
loadFile name go = LoadSalakT $ do
  SourcePack{..} <- get
  (s', e) <- lift $ runWriterT $ go packId source
  put $ SourcePack prefix (packId+1) s' (MI.insert packId name reEnv) (errs ++ e)

tryLoadFile :: MonadIO m => (FilePath -> LoadSalakT m ()) -> FilePath -> LoadSalakT m ()
tryLoadFile f file = do
  b <- liftIO $ doesFileExist file
  when b $ do
    liftIO $ putStrLn $ "Load " <> file
    f file

loading
  :: (Foldable f, Monad m)
  => Reload
  -> f a
  -> (Priority -> a -> m (Text, Value))
  -> LoadSalakT m ()
loading name fa f = loadFile name (\i s -> foldM (go i) s fa)
  where
    go i s a = do
      (k,v) <- lift $ f i a
      insert k v s

-- | Put key value pairs into `SourcePack`
loadMock :: Monad m => [(Text, Text)] -> LoadSalakT m ()
loadMock fs = loading (emptyReload "Mock") fs (\i (k,v) -> return (k, VStr i v))

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



