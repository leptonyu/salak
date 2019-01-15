{-# LANGUAGE CPP #-}
module Data.Salak.Yaml where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Salak.Aeson
import           Data.Salak.Types
import           Data.Yaml

-- | Load `Properties` from `Yaml` file.
makePropertiesFromYaml :: FilePath -> Properties -> IO Properties
makePropertiesFromYaml file p = do

#if __GLASGOW_HASKELL__ <= 710
  x <- decodeFile file
  v <- case x of
    Just a -> return a
    _      -> error $ "load " ++ file ++ " failed"
#else
  v <- decodeFileThrow file
#endif

  return $ makePropertiesFromJson v p

-- | Load Properties from Yaml
--
-- @since 0.2.2
loadYaml :: MonadIO m => FilePath -> LoadProperties m ()
loadYaml file = do
  p <- get
  q <- liftIO $ makePropertiesFromYaml file p
  put q

-- | Load Properties from Yaml if exists
--
-- @since 0.2.2
loadYamlIfExists :: MonadIO m => Maybe FilePath -> LoadProperties m ()
loadYamlIfExists mf = loadIfExists mf loadYaml
