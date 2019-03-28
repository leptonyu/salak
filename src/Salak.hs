{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
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
    defaultLoadSalak
  , loadSalak
  , PropConfig(..)
  -- * Static Load
  , HasSourcePack(..)
  , fetch
  , require
  -- * Dynamic Load
  , ReloadableSourcePack
  , ReloadableSourcePackT
  , ReloadResult(..)
  , reloadable
  , fetchD
  , requireD
  -- * Prop Parser
  , Prop
  , FromProp(..)
  , FromEnumProp(..)
  , (.?=)
  -- * SourcePack
  , SourcePack
  , SourcePackT
  , loadYaml
  , loadCommandLine
  , loadEnv
  , loadMock
  , defaultParseCommandLine
  , ParseCommandLine
  , Priority
  , Value(..)
  ) where

import           Control.Applicative
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Default
import           Data.Text              (Text)
import           Salak.Dynamic
import           Salak.Env
import           Salak.Json
import           Salak.Prop
import           Salak.Types
import           System.Directory
import           System.FilePath        ((</>))

-- | Prop load configuration
data PropConfig = PropConfig
  { configName    :: Maybe String  -- ^ Config name
  , configDirKey  :: Text          -- ^ Specify config dir
  , searchCurrent :: Bool          -- ^ Search current directory, default true
  , searchHome    :: Bool          -- ^ Search home directory, default false.
  , commandLine   :: ParseCommandLine -- ^ How to parse commandline
  }

instance Default PropConfig where
  def = PropConfig Nothing "salak.conf" True False defaultParseCommandLine

-- | Load salak `SourcePack` and fetch properties.
loadSalak
  :: Monad m
  => ReaderT SourcePack m a -- ^ Fetch properties monad.
  -> SourcePackT m ()       -- ^ Load properties monad.
  -> m a
loadSalak a spm = do
  (es, sp) <- runSourcePackT spm
  unless (null es) $ fail (head es)
  runReaderT a sp

-- | Default load salak.
-- All these configuration sources has orders, from highest order to lowest order:
--
-- > 1. CommandLine
-- > 2. Environment
-- > 3. Specified Yaml file(file in `configDirKey`)
-- > 4. Yaml file in current directory
-- > 5. Yaml file in home directory
--
defaultLoadSalak :: MonadIO m => PropConfig -> ReaderT SourcePack m a -> m a
defaultLoadSalak PropConfig{..} a = loadSalak a $ do
  loadCommandLine commandLine
  loadEnv
  cf <- fetch configDirKey
  maybe (return ()) (go cf) configName
  where
    go ck n = do
      case ck of
        Left  _ -> return ()
        Right d -> loadYaml $ d </> n
      c <- liftIO getCurrentDirectory
      when searchCurrent $ tryLoadYaml $ c </> n
      h <- liftIO getHomeDirectory
      when searchHome    $ tryLoadYaml $ h </> n

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
  :: (MonadIO m, HasSourcePack m, FromProp a)
  => Text -- ^ Properties key
  -> ReloadableSourcePackT m (IO a)
requireD k = do
  x <- fetchD k
  case x of
    Left  e -> fail e
    Right v -> return v

-- | Try fetch dynamic properties from `SourcePack`
fetchD
  :: (MonadIO m, HasSourcePack m, FromProp a)
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

-- $use
--
-- | This library default a standard configuration load process. It can load properties from `CommandLine`, `Environment`,
-- `JSON value` and `Yaml` files. They all load to the same format `SourcePack`. Earler property source has higher order
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
-- > instance FromProp Config where
-- >   fromProp = Config
-- >     <$> "user"
-- >     <*> "pwd"
-- >     <*> "ext" .?= 1
--
-- > main = do
-- >   c :: Config <- defaultLoadSalak def $ require ""
-- >   print c
--
-- > λ> c
-- > Config {name = "daniel", dir = Nothing, ext = 1}
