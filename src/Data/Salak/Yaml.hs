module Data.Salak.Yaml where

import           Data.Salak.Aeson
import           Data.Salak.Types
import           Data.Yaml

-- | Load `Properties` from `Yaml` file.
makePropertiesFromYaml :: FilePath -> Properties -> IO Properties
makePropertiesFromYaml file p = do
  v <- decodeFileThrow file
  return $ makePropertiesFromJson v p
