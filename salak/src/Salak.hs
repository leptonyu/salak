{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoOverloadedLists   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module:      Salak
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Configuration Loader for Production in Haskell.
--
module Salak(
  -- * How to use this library
  -- $use

  -- * Salak Main Functions
    runSalak
  , runSalakWith
  , PropConfig(..)
  -- ** Basic Load
  , loadCommandLine
  , ParseCommandLine
  , defaultParseCommandLine
  , loadEnv
  , loadMock
  -- ** Load Extensions
  , ExtLoad
  , loadByExt
  , HasLoad(..)
  , (:|:)(..)
  -- * Salak Monad
  , RunSalak
  , HasSalak(..)
  -- ** Get Properties
  , require
  , requireD
  -- ** Reload Result
  , UpdateResult(..)
  , receive
  , receive_
  -- * Properties Parsers
  , (.?=)
  , (.?:)
  , FromProp(..)
  , readPrimitive
  , PResult(..)
  ) where

import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Data.Default
import           Data.Text               (Text)
import           Salak.Internal
import           Salak.Internal.Prop
import           System.Directory
import           System.FilePath         ((</>))

-- | Prop load configuration
data PropConfig = PropConfig
  { configName    :: Maybe String  -- ^ Config name
  , configDirKey  :: Text          -- ^ Specify config dir
  , searchCurrent :: Bool          -- ^ Search current directory, default true
  , searchHome    :: Bool          -- ^ Search home directory, default false.
  , commandLine   :: ParseCommandLine -- ^ How to parse commandline
  , loadExt       :: FilePath -> RunSalak ()
  }

instance Default PropConfig where
  def = PropConfig
    Nothing
    "salak.conf.dir"
    True
    False
    defaultParseCommandLine
    (\_ -> return ())

-- | Load file by extension
type ExtLoad = (String, FilePath -> RunSalak ())

class HasLoad a where
  loaders :: a -> [ExtLoad]

data a :|: b = a :|: b
infixr 3 :|:

instance (HasLoad a, HasLoad b) => HasLoad (a :|: b) where
  loaders (a :|: b) = loaders a ++ loaders b

-- | Load files with specified format, yaml or toml, etc.
loadByExt :: HasLoad a => a -> FilePath -> RunSalakT IO ()
loadByExt xs f = mapM_ go (loaders xs)
  where
    go (ext, ly) = tryLoadFile ly $ f ++ "." ++ ext

-- | Default run salak.
-- All these configuration sources has orders, from highest priority to lowest priority:
--
-- > 1. loadCommandLine
-- > 2. loadEnvironment
-- > 3. loadConfFiles
-- > 4. load file from folder `salak.conf.dir` if defined
-- > 5. load file from current folder if enabled
-- > 6. load file from home folder if enabled
-- > 7. file extension matching, support yaml or toml or any other loader.
--
loadSalak :: PropConfig -> RunSalak ()
loadSalak PropConfig{..} = do
  loadCommandLine commandLine
  loadEnv
  forM_ configName $ forM_
    [ require configDirKey
    , ifS searchCurrent getCurrentDirectory
    , ifS searchHome    getHomeDirectory
    ] . loadConf
  where
    ifS True gxd = Just <$> liftIO gxd
    ifS _    _   = return Nothing
    loadConf n mf = mf >>= mapM_ (loadExt . (</> n))

-- | Default run salak.
-- All these configuration sources has orders, from highest priority to lowest priority:
--
-- > 1. loadCommandLine
-- > 2. loadEnvironment
-- > 3. loadConfFiles
-- > 4. load file from folder `salak.conf.dir` if defined
-- > 5. load file from current folder if enabled
-- > 6. load file from home folder if enabled
-- > 7. file extension matching, support yaml or toml or any other loader.
--
runSalak :: PropConfig -> RunSalak (IO UpdateResult -> IO a) -> IO a
runSalak p sa = runTrie $ loadSalak p >> sa

-- | Simplified run salak, should specified code name and file format.
runSalakWith :: HasLoad file => String -> file -> RunSalak (IO UpdateResult -> IO a) -> IO a
runSalakWith name f sa = runTrie $ loadSalakFile >> sa
  where
    loadSalakFile = loadSalak def { configName = Just name, loadExt = loadByExt f}

-- | Fetch properties from `Source`, or throw fail
require
  :: (HasSalak m, FromProp a)
  => Text -- ^ Properties key
  -> m a
require k = searchTrie k >>= either error return

-- | Fetch dynamic properties from `Source`, or throw fail
requireD
  :: (MonadIO m, FromProp a)
  => Text -- ^ Properties key
  -> RunSalakT m (IO a)
requireD k = fetchTrie k >>= either error return

-- $use
--
-- | This library define a universal procedure to load configurations and parse properties, also supports reload configuration files.
--
--
-- We can load configurations from command line, environment, configuration files such as yaml or toml etc, and we may want to have our own strategies to load configurations from multi sources and overwrite properties by orders of these sources.
--
-- `PropConfig` defines a common loading strategy:
--
-- > 1. loadCommandLine
-- > 2. loadEnvironment
-- > 3. loadConfFiles
-- > 4. load file from folder `salak.conf.dir` if defined
-- > 5. load file from current folder if enabled
-- > 6. load file from home folder if enabled
-- > 7. file extension matching, support yaml or toml or any other loader.
--
-- Load earlier has higher priority, priorities cannot be changed.
--
--
-- Usage:
--
--
-- Environment:
--
-- > export TEST_CONFIG_NAME=daniel
--
-- Current Directory:  salak.yaml
--
-- > test.config:
-- >   name: noop
-- >   dir: ls
--
-- Current Directory:  salak.toml
--
-- > [test.config]
-- > ext=2
--
-- > data Config = Config
-- >   { name :: Text
-- >   , dir  :: Maybe Text
-- >   , ext  :: Int
-- >   } deriving (Eq, Show)
-- >
-- > instance FromProp Config where
-- >   fromProp = Config
-- >     <$> "user" ? pattern "[a-z]{5,16}"
-- >     <*> "pwd"
-- >     <*> "ext" .?= 1
-- >
-- > main = runSalakWith "salak" (YAML :|: TOML) $ do
-- >   c :: Config <- require "test.config"
-- >   lift $ print c
-- >   receive_
--
-- GHCi play
--
-- > λ> import Salak
-- > λ> import Salak.Internal
-- > λ> import Data.Text(Text)
-- > λ> data Config = Config { name :: Text, dir  :: Maybe Text, ext  :: Int} deriving (Eq, Show)
-- > λ> instance FromProp Config where fromProp = Config <$> "user" <*> "dir" <*> "ext" .?= 1
-- > λ> runSalak def (receive $ require "") :: IO Config
-- > Config {name = "daniel", dir = Just "ls", ext = 2}
--
