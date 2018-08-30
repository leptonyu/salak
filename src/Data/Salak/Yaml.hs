module Data.Salak.Yaml where

import           Data.Salak.Aeson
import           Data.Salak.Property
import           Data.Yaml

makePropertiesFromYaml :: FilePath -> Properties -> IO Properties
makePropertiesFromYaml file p = do
  v <- decodeFileThrow file
  return $ makePropertiesFromJson v p
