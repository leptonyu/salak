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
    loadAndRunSalak
  , runSalak
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
  , loadCommandLine
  , loadEnv
  , loadMock
  , loadYaml
  , loadToml
  , loadByExt
  , defaultLoadByExt
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
import           Salak.Load.Dynamic
import           Salak.Load.Env
import           Salak.Load.Json
import           Salak.Load.Toml
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
    defaultLoadByExt

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

type ExtLoad = (String, FilePath -> SourcePackT IO ())

loadByExt :: MonadIO m => [ExtLoad] -> FilePath -> SourcePackT m ()
loadByExt xs f = mapM_ go xs
  where
    go (ext, ly) = tryLoadFile (jump . ly) $ f ++ "." ++ ext

jump :: MonadIO m => StateT SourcePack IO a -> StateT SourcePack m ()
jump a = get >>= lift . liftIO . execStateT a >>= put

defaultLoadByExt :: MonadIO m => FilePath -> SourcePackT m ()
defaultLoadByExt = loadByExt
  [ ("yaml", loadYaml)
  , ("yml",  loadYaml)
  , ("toml", loadToml)
  , ("tml",  loadToml)
  ]

-- | Default load salak.
-- All these configuration sources has orders, from highest order to lowest order:
--
-- > 1. CommandLine
-- > 2. Environment
-- > 3. Specified Yaml file(file in `configDirKey`)
-- > 4. Yaml file in current directory
-- > 5. Yaml file in home directory
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
