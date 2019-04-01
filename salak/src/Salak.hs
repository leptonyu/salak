{-# LANGUAGE FlexibleInstances   #-}
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

  -- * Salak
    loadAndRunSalak
  , runSalak
  , PropConfig(..)
  -- * Static Get Properties
  , HasSourcePack(..)
  , fetch
  , require
  -- * Dynamic Get Properties
  , ReloadableSourcePack
  , ReloadableSourcePackT
  , ReloadResult(..)
  , reloadable
  , reloadAction
  , fetchD
  , requireD
  -- * Prop Parser
  , Prop
  , FromProp(..)
  , FromEnumProp(..)
  , (.?=)
  , (.?:)
  -- * SourcePack
  , SourcePack
  , SourcePackT
  -- * Load configurations
  , loadCommandLine
  , loadEnv
  , loadMock
  -- ** Load By Extension
  , ExtLoad
  , loadByExt
  , HasLoad(..)
  , (:|:)(..)
  -- * Other
  , ParseCommandLine
  , defaultParseCommandLine
  , Priority
  , Value(..)
  , defaultLoadSalak
  ) where

import           Control.Applicative
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Default
import           Data.Text              (Text)
import           Salak.Load.Dynamic
import           Salak.Load.Env
import           Salak.Prop
import           Salak.Types
import           Salak.Types.Value
import           System.Directory
import           System.FilePath        ((</>))

-- | Prop load configuration
data PropConfig = PropConfig
  { configName    :: Maybe String  -- ^ Config name
  , configDirKey  :: Text          -- ^ Specify config dir
  , searchCurrent :: Bool          -- ^ Search current directory, default true
  , searchHome    :: Bool          -- ^ Search home directory, default false.
  , commandLine   :: ParseCommandLine -- ^ How to parse commandline
  , loadExt       :: FilePath -> SourcePackT IO ()
  }

instance Default PropConfig where
  def = PropConfig
    Nothing
    "salak.conf.dir"
    True
    False
    defaultParseCommandLine
    (\_ -> return ())

-- | Load and run salak `SourcePack` and fetch properties.
loadAndRunSalak
  :: Monad m
  => SourcePackT m ()       -- ^ Load properties monad.
  -> ReaderT SourcePack m a -- ^ Fetch properties monad.
  -> m a
loadAndRunSalak spm a = do
  sp <- runSourcePackT spm
  let es = errs sp
  unless (null es) $ fail (head es)
  runReaderT a sp


-- | Load file by extension
type ExtLoad = (String, FilePath -> SourcePackT IO ())

class HasLoad a where
  loaders :: a -> [ExtLoad]

data a :|: b = a :|: b
infixr 3 :|:

instance (HasLoad a, HasLoad b) => HasLoad (a :|: b) where
  loaders (a :|: b) = loaders a ++ loaders b

loadByExt :: (HasLoad a, MonadIO m) => a -> FilePath -> SourcePackT m ()
loadByExt xs f = mapM_ go (loaders xs)
  where
    go (ext, ly) = tryLoadFile (jump . ly) $ f ++ "." ++ ext

jump :: MonadIO m => StateT SourcePack IO a -> StateT SourcePack m ()
jump a = get >>= lift . liftIO . execStateT a >>= put

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
runSalak :: MonadIO m => PropConfig -> ReaderT SourcePack m a -> m a
runSalak PropConfig{..} = loadAndRunSalak $ do
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
    loadConf n mf = mf >>= mapM_ (jump . loadExt . (</> n))

{-# DEPRECATED defaultLoadSalak "use runSalak instead" #-}
defaultLoadSalak :: MonadIO m => PropConfig -> ReaderT SourcePack m a -> m a
defaultLoadSalak = runSalak


  --   loadC ffa n = (mapM_ . mapM_) g
  -- cf <- fetch configDirKey
  -- where
  --   go ck n = do
  --     case ck of
  --       Left  _ -> return ()
  --       Right d -> defaultLoadByExt $ d </> n
  --     c <- liftIO getCurrentDirectory
  --     when searchCurrent $ defaultLoadByExt $ c </> n
  --     h <- liftIO getHomeDirectory
  --     when searchHome    $ defaultLoadByExt $ h </> n

class Monad m => HasSourcePack m where
  askSourcePack :: m SourcePack

instance Monad m => HasSourcePack (ReaderT SourcePack m) where
  askSourcePack = ask
instance Monad m => HasSourcePack (StateT SourcePack m) where
  askSourcePack = get

-- | Try fetch properties from `SourcePack`
fetch
  :: (HasSourcePack m, FromProp a)
  => Text -- ^ Properties key
  -> m (Either String a)
fetch key = search key <$> askSourcePack

-- | Fetch properties from `SourcePack`, or throw fail
require
  :: (HasSourcePack m, FromProp a)
  => Text -- ^ Properties key
  -> m a
require k = do
  x <- fetch k
  case x of
    Left  e -> fail e
    Right v -> return v

-- | Fetch dynamic properties from `SourcePack`, or throw fail
requireD
  :: (MonadIO m, FromProp a)
  => Text -- ^ Properties key
  -> ReloadableSourcePackT m (IO a)
requireD k = do
  x <- fetchD k
  case x of
    Left  e -> fail e
    Right v -> return v

-- | Try fetch dynamic properties from `SourcePack`
fetchD
  :: (MonadIO m, FromProp a)
  => Text -- ^ Properties key
  -> ReloadableSourcePackT m (Either String (IO a))
fetchD = search'

-- | Lift to reloadable environment for dynamic properties.
reloadable :: (MonadIO m, HasSourcePack m) => ReloadableSourcePackT m a -> m a
reloadable f = askSourcePack >>= runReloadable f

-- | Optional value.
infixl 5 .?=
(.?=) :: Alternative f => f a -> a -> f a
(.?=) a b = a <|> pure b

-- | Default value.
infixl 5 .?:
(.?:) :: (Alternative f, Default b) => f a -> (b -> a) -> f a
(.?:) fa b = fa .?= b def

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
-- > main = runSalak def { configName = Just "salak", loadExt = loadByExt $ YAML :|: TOML } $ do
-- >   c :: Config <- require "test.config"
-- >   lift $ print c
--
-- GHCi play
--
-- > λ> import Salak
-- > λ> import Salak.Load.YAML
-- > λ> import Salak.Load.TOML
-- > λ> :set -XTypeApplications
-- > λ> instance FromProp Config where fromProp = Config <$> "user" <*> "dir" <*> "ext" .?= 1
-- > λ> f = runSalak def { configName = Just "salak", loadExt = loadByExt $ YAML :|: TOML }
-- > λ> f (require "") >>= print @Config
-- > Config {name = "daniel", dir = Just "ls", ext = 2}
--
