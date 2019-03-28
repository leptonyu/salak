{-# LANGUAGE FlexibleInstances #-}
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

  -- * Source
    Selector
  , SourcePack
  , SourcePackT
  , Reload(..)
  , load
  , loadJSON
  , loadYaml
  , loadEnv
  -- * Salak
  , defaultLoadSalak
  , loadSalak
  , PropConfig(..)
  , HasSourcePack(..)
  , fetch
  , require
  -- * Utilities
  , ReloadableSourcePack
  , runReloadable
  , Prop
  , FromProp(..)
  , PResult(..)
  , readPrimitive
  , readSelect
  , err
  ) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader
import           Data.Default
import           Data.Text              (Text)
import           Salak.Dynamic
import           Salak.Env
import           Salak.Json
import           Salak.Prop
import           Salak.Types

data PropConfig = PropConfig
  { configFileKey :: Maybe String
  , commandLine   :: ParseCommandLine
  }

instance Default PropConfig where
  def = PropConfig Nothing defaultParseCommandLine

loadSalak :: Monad m => ReaderT SourcePack m a -> SourcePackT m () -> m a
loadSalak a spm = do
  (es, sp) <- runSourcePackT spm
  unless (null es) $ do
    fail (head es)
  runReaderT a sp

defaultLoadSalak :: MonadIO m => PropConfig -> ReaderT SourcePack m a -> m a
defaultLoadSalak PropConfig{..} a = loadSalak a $ do
  loadCommandLine commandLine
  loadEnv
  go configFileKey
  where
    go (Just file) = loadYaml file
    go _           = return ()

class Monad m => HasSourcePack m where
  askSourcePack :: m SourcePack

fetch :: (HasSourcePack m, FromProp a) => Text -> m (Either String a)
fetch key = askSourcePack >>= return . search key

require :: (HasSourcePack m, FromProp a) => Text -> m a
require k = do
  x <- fetch k
  case x of
    Left  e -> fail e
    Right v -> return v

instance Monad m => HasSourcePack (ReaderT SourcePack m) where
  askSourcePack = ask

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
