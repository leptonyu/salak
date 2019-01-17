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
    LoadProperties
  , runLoad
  , askProperties
  , setValue
  , loadCommandLine
  , loadEnvironment
  , loadJSON
  , loadYaml
  , loadYamlIfExists
  -- ** Predefined Loaders
  , defaultProperties
  , defaultProperties'
  , defaultPropertiesWithFile
  , defaultPropertiesWithFile'
  , empty
  -- * Lookup Properties
  , lookup
  , toKeys
  -- * Types
  , Property(..)
  , Properties(..)
  , FromProperties(..)
  , Return
  -- * Properties Loader Helper
  , insert
  , defaultParseCommandLine
  , ParseCommandLine
  , FileName
  -- * Operations
  , module Data.Salak.Operation
  ) where

import           Control.Monad.Trans.Class (lift)
import           Data.Maybe
import           Data.Salak.Aeson
import           Data.Salak.CommandLine
import           Data.Salak.Environment
import           Data.Salak.Operation
import           Data.Salak.Types
import           Data.Salak.Yaml
import           Data.Text                 (pack)
import           Prelude                   hiding (lookup)
import           System.Directory
import           System.FilePath           ((</>))

-- | Initialize default properties from `CommandLine` and `Environment`.
-- `CommandLine` use default parser.
defaultProperties :: IO Properties
defaultProperties = defaultProperties' defaultParseCommandLine

-- | Initialize default properties from `CommandLine` and `Environment`.
defaultProperties' :: ParseCommandLine -> IO Properties
defaultProperties' dpc = runLoad $ do
  loadCommandLine dpc
  loadEnvironment

-- | Yaml file name.
type FileName = String

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
defaultPropertiesWithFile' name dpc = runLoad $ do
  loadCommandLine dpc
  loadEnvironment
  p <- askProperties
  let n  = fromMaybe name $ p .>> "salak.config.name"
      f  = p .>> "salak.config.dir"
  setValue "salak.config.name" (PStr $ pack n)
  case f of
    Just y -> loadYaml (y </> n)
    _      -> return ()
  c <- lift getCurrentDirectory
  loadYamlIfExists (Just $ c </> n)
  h <- lift getHomeDirectory
  loadYamlIfExists (Just $ h </> n)

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
-- > instance FromProperties Config where
-- >   fromProperties v = Config
-- >         <$> v .?> "name"
-- >         <*> v .?> "dir"
-- >         <*> v .?> "ext" .?= 1
--
-- > main = do
-- >   p <- defaultPropertiesWithFile "salak.yml"
-- >   let config  = p .>> "salak.config"  :: Config
-- >       enabled = p .?> "salak.enabled" .|= True
-- >   print config
-- >   print enabled

