{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Salak
-- Copyright:   (c) 2018 Daniel YU
-- License:     BSD3
-- Maintainer:  Daniel YU <leptonyu@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Configuration Loader for Production in Haskell.
--
module Data.Salak(
  -- * How to use this library
  -- $use

  -- * Properties Loader
    defaultProperties
  , ParseCommandLine
  , defaultProperties'
  , defaultPropertiesWithFile
  , defaultPropertiesWithFile'
  , empty
  -- * Lookup Properties
  , lookup
  , lookup'
  , toKeys
  -- * Types
  , Property(..)
  , Properties(..)
  , Key
  , FromProperties(..)
  , Return(..)
  -- * Properties Loader Helper
  , insert
  , makePropertiesFromEnvironment
  , defaultParseCommandLine
  , makePropertiesFromCommandLine
  , makePropertiesFromJson
  , makePropertiesFromYaml
  , FileName
  -- * Operations
  , module Data.Salak.Operation
  ) where

import           Data.Maybe
import           Data.Salak.Aeson
import           Data.Salak.CommandLine
import           Data.Salak.Environment
import           Data.Salak.Operation
import           Data.Salak.Types
import           Data.Salak.Yaml
import           Data.Text              (Text, unpack)
import           Prelude                hiding (lookup)
import           System.Directory
import           System.FilePath        ((</>))

-- | Initialize default properties from `CommandLine` and `Environment`.
-- `CommandLine` use default parser.
defaultProperties :: IO Properties
defaultProperties = defaultProperties' defaultParseCommandLine

-- | Initialize default properties from `CommandLine` and `Environment`.
defaultProperties' :: ParseCommandLine -> IO Properties
defaultProperties' dpc
  = makePropertiesFromCommandLine dpc empty
  >>= makePropertiesFromEnvironment

-- | Yaml file name.
type FileName = Text

-- | Initialize default properties from `CommandLine`, `Environment` and `Yaml` files.
-- All these configuration sources has orders, from highest order to lowest order:
--
-- > 1. CommandLine
-- > 2. Environment
-- > 3. Specified Yaml file(file in "salak.config.dir")
-- > 4. Yaml file in current directory
-- > 5. Yaml file in home directory
--
defaultPropertiesWithFile
  :: FileName -- ^ specify default config file name, can reset by config "salak.config.name" from `CommandLine` or `Environment`.
  -> IO Properties
defaultPropertiesWithFile name = defaultPropertiesWithFile' name defaultParseCommandLine

-- | Initialize default properties from `CommandLine`, `Environment` and `Yaml` files.
-- All these configuration sources has orders, from highest order to lowest order:
--
-- > 1. CommandLine
-- > 2. Environment
-- > 3. Specified Yaml file(file in "salak.config.dir")
-- > 4. Yaml file in current directory
-- > 5. Yaml file in home directory
--
defaultPropertiesWithFile'
  :: FileName -- ^ specify default config file name, can reset by config "salak.config.name" from `CommandLine` or `Environment`.
  -> ParseCommandLine -- ^ parser for command line
  -> IO Properties
defaultPropertiesWithFile' name dpc = do
  p <- defaultProperties' dpc
  let n  = fromMaybe name $ lookup "salak.config.name" p
      p' = insert (toKeys "salak.config.name") (PStr n) p
  c <- getCurrentDirectory
  h <- getHomeDirectory
  foldl (go n) (return p') [(lookup "salak.config.dir" p, False), (Just c, True), (Just h, True)]
  where
    go _ p (Nothing, _) = p
    go n p (Just d, ok) = let f = d </> unpack n in do
      p' <- p
      b <- doesFileExist f
      if b
        then makePropertiesFromYaml f p'
        else if ok then return p' else error $ "File " ++ f ++  " not found"

-- $use
--
-- | This library default a standard configuration load process. It can load properties from `CommandLine`, `Environment`,
-- `JSON value` and `Yaml` files. They all load to the same format `Properties`. Earler property source has higher order
-- to load property. For example:
--
-- > CommandLine:  --package.a.enabled=true
-- > Environment: PACKAGE_A_ENABLED: false
--
-- > lookup "package.a.enabled" properties => Just True
--
-- `CommandLine` has higher order then `Environment`, for the former load properties earler then later.
--
-- Usage:
--
-- > data Config = Config
-- >   { name :: Text
-- >   , dir  :: Maybe Text
-- >   , ext  :: Int
-- >   } deriving (Eq, Show)
-- >
-- > instance FromJSON Config where
-- >   parseJSON = withObject "Config" $ \v -> Config
-- >         <$> v .:  "name"
-- >         <*> v .:? "dir"
-- >         <*> (fromMaybe 1 <$> v .:? "ext")
--
-- > main = do
-- >   p <- defaultPropertiesWithFile "salak.yml"
-- >   let Just config = lookup "salak.config" p :: Maybe Config
-- >   print config

