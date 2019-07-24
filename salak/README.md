# salak

[![Hackage](https://img.shields.io/hackage/v/salak.svg)](https://hackage.haskell.org/package/salak)
[![stackage LTS package](http://stackage.org/package/salak/badge/lts)](http://stackage.org/lts/package/salak)
[![stackage Nightly package](http://stackage.org/package/salak/badge/nightly)](http://stackage.org/nightly/package/salak)
[![Build Status](https://travis-ci.org/leptonyu/salak.svg?branch=master)](https://travis-ci.org/leptonyu/salak)

Configuration (re)loader in Haskell.

## salak-yaml
[![salak-yaml](https://img.shields.io/hackage/v/salak-yaml.svg)](https://hackage.haskell.org/package/salak-yaml)
## salak-toml
[![salak-toml](https://img.shields.io/hackage/v/salak-toml.svg)](https://hackage.haskell.org/package/salak-toml)

This library define a universal procedure to load configurations and parse properties, also supports reload configuration files.

We can load configurations from command line, environment, configuration files such as yaml or toml etc, and we may want to have our own strategies to load configurations from multi sources and overwrite properties by orders of these sources.

`PropConfig` defines a common loading strategy:
> 1. loadCommandLine
> 2. loadEnvironment
> 3. loadConfFiles
> 4. load file from folder `salak.conf.dir` if defined
> 5. load file from current folder if enabled
> 6. load file from home folder if enabled
> 7. file extension matching, support yaml or toml or any other loader.

Load earlier has higher orders, orders cannot be changed.

`ReaderT SourcePack m` defines how to read properties:
```Haskell
require "abc.prop"
```

`ReloadableSourcePackT m` defines how to read reloadable properties:
```Haskell
requireD "abc.dynamic.prop"
```

For commandline and environment, 
```
CommandLine:  --package.a.enabled=true
Environment: PACKAGE_A_ENABLED: false
```

Usage:


Environment:
```
export TEST_CONFIG_NAME=daniel
```
Current Directory:  salak.yaml
```YAML
test.config:
  name: noop
  dir: ls
```
Current Directory:  salak.toml
```TOML
[test.config]
ext=2
```

```Haskell
data Config = Config
  { name :: Text
  , dir  :: Maybe Text
  , ext  :: Int
  } deriving (Eq, Show)

instance MonadCatch m => FromProp m Config where
  fromProp = Config
    <$> "user" ? pattern "[a-z]{5,16}"
    <*> "pwd"
    <*> "ext" .?= 1

main = runSalakWith "salak" (YAML :|: TOML) $ do
  c :: Config <- require "test.config"
  lift $ print c=
```

GHCi play
```Haskell
λ> :set -XFlexibleInstances -XMultiParamTypeClasses 
λ> import Salak
λ> import Data.Default
λ> import Data.Text(Text)
λ> data Config = Config { name :: Text, dir  :: Maybe Text, ext  :: Int} deriving (Eq, Show)
λ> instance MonadCatch m => FromProp m Config where fromProp = Config <$> "user" <*> "dir" <*> "ext" .?= 1
λ> runSalak def (require "") :: IO Config
Config {name = "daniel", dir = Nothing, ext = 1}
```

TODO:
- Recover placeholder
- Add git pull support.
- Add automatic reloading.
