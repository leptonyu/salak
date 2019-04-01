{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Salak.Types where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.IntMap.Strict   as MI
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

defReload :: String -> SourcePackT IO () -> Reload
defReload s spt = Reload (T.pack s) (\i -> go <$> execStateT spt emptySourcePack {packId = i})
  where
    go SourcePack{..} = (errs, source)

emptyReload s = defReload s (return ())

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

addErr' :: String -> SourcePack -> SourcePack
addErr' e sp = sp {errs = e : errs sp}

loadFile :: Reload -> SourcePack -> (Priority -> Source -> Writer [String] Source) -> SourcePack
loadFile name SourcePack{..} go =
  let (s', e) = runWriter $ go packId source
  in SourcePack prefix (packId+1) s' (MI.insert packId name reEnv) (errs ++ e)

tryLoadFile :: MonadIO m => (FilePath -> SourcePackT m ()) -> FilePath -> SourcePackT m ()
tryLoadFile f file = do
  b <- liftIO $ doesFileExist file
  when b $ do
    liftIO $ putStrLn $ "Load " <> file
    f file

load
  :: Foldable f
  => Reload
  -> f a
  -> (Priority -> a -> (Text, Value))
  -> SourcePack
  -> SourcePack
load name fa f sp = loadFile name sp $ \i s -> foldl (go i) (return s) fa
  where
    go i s a = s >>= \s' -> let (k,v) = f i a in insert k v s'

loadMock :: Monad m => [(Text, Text)] -> SourcePackT m ()
loadMock fs = modify $ load (emptyReload "Mock") fs (\i (k,v) -> (k, VStr i v))

type SourcePackT = StateT SourcePack

runSourcePackT :: Monad m => SourcePackT m a -> m SourcePack
runSourcePackT ac = execStateT ac emptySourcePack

