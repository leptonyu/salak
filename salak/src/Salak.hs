{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoOverloadedLists     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module:      Salak
-- Copyright:   2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Configuration (re)Loader and Parser.
--
module Salak(
  -- * How to use this library
  -- $use

  -- * Salak Main Functions
    runSalak
  , runSalakWith
  , loadAndRunSalak
  , loadAndRunSalak'
  , PropConfig(..)
  -- * Parsing Properties Function
  , MonadSalak(..)
  , RunSalakT
  , RunSalak
  -- ** Operators
  , PropOp(..)
  , FromProp(..)
  , Prop
  , readPrimitive
  , readEnum
  , SourcePack
  , Salak
  , SalakException(..)
  , module Salak.Internal.Writable
  -- * Load Functions
  -- ** Monad for Loader
  , LoadSalakT
  , LoadSalak
  -- ** Basic loaders
  , loadCommandLine
  , ParseCommandLine
  , defaultParseCommandLine
  , loadEnv
  , loadMock
  , loadSalak
  , loadSalakWith
  -- ** File Loaders
  , ExtLoad
  , loadByExt
  , HasLoad(..)
  , (:|:)(..)
  -- ** Reload Functions
  , ReloadResult(..)
  -- * Reexport
  , MonadCatch
  , MonadThrow
  , MonadIO
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Data.Default
import           Data.Maybe
import           Data.Text               (Text)
import           Salak.Internal
import           Salak.Internal.Prop
import           Salak.Internal.Source
import           Salak.Internal.Writable
import           System.Directory
import           System.FilePath         ((</>))

-- | Type synonyms of 'SourcePack'
type Salak = SourcePack

-- | Prop load configuration
data PropConfig = PropConfig
  { configKey     :: Text          -- ^ Specify config key, default is @application@.
  , configName    :: String        -- ^ Specify config name, default is @application@.
  , searchCurrent :: Bool          -- ^ Search current directory, default true.
  , searchHome    :: Bool          -- ^ Search home directory, default false.
  , commandLine   :: ParseCommandLine -- ^ How to parse commandline.
  , loggerF       :: LFunc
  , loadExt       :: FilePath -> LoadSalak ()
  }

instance Default PropConfig where
  def = PropConfig
    "application"
    "application"
    True
    False
    defaultParseCommandLine
    putStrLn
    (\_ -> return ())

data FileConfig = FileConfig
  { configNm  :: Maybe String
  , configDir :: Maybe FilePath
  }

instance Monad m => FromProp m FileConfig where
  fromProp = FileConfig
    <$> "name" .?= Nothing
    <*> "dir"  .?= Nothing

-- | Load file by extension
type ExtLoad = (String, FilePath -> LoadSalak ())

class HasLoad a where
  loaders :: a -> [ExtLoad]

data a :|: b = a :|: b
infixr 3 :|:

instance (HasLoad a, HasLoad b) => HasLoad (a :|: b) where
  loaders (a :|: b) = loaders a ++ loaders b

-- | Load files with specified format, yaml or toml, etc.
loadByExt :: HasLoad a => a -> FilePath -> LoadSalak ()
loadByExt xs f = mapM_ go (loaders xs)
  where
    go (ext, ly) = tryLoadFile ly $ f ++ "." ++ ext

-- | Default load salak.
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
loadSalak :: (MonadThrow m, MonadIO m) => PropConfig -> LoadSalakT m ()
loadSalak PropConfig{..} = do
  setLogF loggerF
  loadCommandLine commandLine
  loadEnv
  FileConfig{..} <- require configKey
  forM_
    [ return configDir
    , ifS searchCurrent getCurrentDirectory
    , ifS searchHome    getHomeDirectory
    ] (loadConf $ fromMaybe configName configNm)
  where
    ifS True gxd = Just <$> liftIO gxd
    ifS _    _   = return Nothing
    loadConf n mf = lift mf >>= mapM_ (liftNT . loadExt . (</> n))

loadSalakWith :: (MonadThrow m, MonadIO m, HasLoad file) => file -> String -> LoadSalakT m ()
loadSalakWith file name = loadSalak def { configName = name, loadExt = loadByExt file }

-- | Standard salak functions, by load and run with `RunSalakT`.
loadAndRunSalak :: (MonadThrow m, MonadIO m) => LoadSalakT m () -> RunSalakT m a -> m a
loadAndRunSalak lstm ma = loadAndRunSalak' lstm $ runRun ma

-- | Run salak, load strategy refer to `loadSalak`
runSalak :: (MonadCatch m, MonadIO m) => PropConfig -> RunSalakT m a -> m a
runSalak c = loadAndRunSalak (loadSalak c)

-- | Run salak, load strategy refer to `loadSalakWith`
runSalakWith :: (MonadCatch m, MonadIO m, HasLoad file) => String -> file -> RunSalakT m a -> m a
runSalakWith name file = loadAndRunSalak (loadSalakWith file name)

-- $use
--
-- | This library defines a universal procedure to load configurations and parse properties, also supports reload configurations.
--
--
-- We can load configurations from command lines, environment, configuration files such as yaml or toml etc.,
-- and we may want to have our own strategies to load configurations from multiply sources and overwrite properties by orders of these sources.
--
-- `PropConfig` defines a common loading strategy:
--
-- > 1. loadCommandLine
-- > 2. loadEnvironment
-- > 3. loadConfFiles
-- > 4. load file from folder `application.dir` if defined
-- > 5. load file from current folder if enabled
-- > 6. load file from home folder if enabled
-- > 7. file extension matching, support yaml or toml or any other loader.
--
-- Load earlier has higher priority. Priorities cannot be changed.
--
-- After loading configurations, we can use `require` to parse properties. For example:
--
-- > a :: Bool              <- require "bool.key"
-- > b :: Maybe Int         <- require "int.optional.key"
-- > c :: Either String Int <- require "int.error.key"
-- > d :: IO Int            <- require "int.reloadable.key"
--
-- Salak supports parse `IO` values, which actually wrap a 'Control.Concurrent.MVar.MVar' variable and can be reseted by reloading configurations.
-- Normal value will not be affected by reloading configurations.
--
-- GHCi play
--
-- >>> :set -XFlexibleInstances -XMultiParamTypeClasses -XOverloadedStrings
-- >>> import Salak
-- >>> import Data.Default
-- >>> import Data.Text(Text)
-- >>> data Config = Config { name :: Text, dir  :: Maybe Text, ext  :: Int} deriving (Eq, Show)
-- >>> instance Monad m => FromProp m Config where fromProp = Config <$> "user" <*> "dir" <*> "ext" .?= 1
-- >>> runSalak def (require "") :: IO Config
-- Config {name = "daniel", dir = Nothing, ext = 1}
--
