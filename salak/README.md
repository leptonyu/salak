# salak

[![Hackage](https://img.shields.io/hackage/v/salak.svg?logo=haskell)](https://hackage.haskell.org/package/salak)
[![Build](https://img.shields.io/travis/leptonyu/salak.svg?logo=travis)](https://travis-ci.com/leptonyu/salak)
[![stackage LTS package](http://stackage.org/package/salak/badge/lts)](http://stackage.org/lts/package/salak)
[![stackage Nightly package](http://stackage.org/package/salak/badge/nightly)](http://stackage.org/nightly/package/salak)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/leptonyu/salak/blob/master/salak/LICENSE)
![Hackage-Deps](https://img.shields.io/hackage-deps/v/salak)


Configuration (re)loader in Haskell.

### Packages

- [![Hackage](https://img.shields.io/badge/salak-yaml-orange)](https://hackage.haskell.org/package/salak-yaml) Yaml support for salak.
- [![Hackage](https://img.shields.io/badge/salak-toml-orange)](https://hackage.haskell.org/package/salak-toml) Toml support for salak.

## Introduction
This library defines a universal procedure to load configurations and parse properties, also supports reload configuration files.

## Parse Functions

`MonadSalak` monad provide a unified function `require` to parse properties. Here are some examples.

```Haskell
a :: Bool              <- require "bool.key"
b :: Maybe Int         <- require "int.optional.key"
c :: Either String Int <- require "int.error.key"
d :: IO Int            <- require "int.reloadable.key" -- This property can be changed by reloading configurations.
```

## Load Strategy
We can load configurations from command lines, environment, configuration files such as yaml or toml etc., and we may want to have our own strategies to load configurations from multiply sources and overwrite properties by orders of these sources.

`PropConfig` defines a common loading strategy:
> 1. loadCommandLine
> 2. loadEnvironment
> 3. loadConfFiles
> 4. load file from folder `application.dir` if defined
> 5. load file from current folder if enabled
> 6. load file from home folder if enabled
> 7. file extension matching, support yaml or toml or any other loader.

Load earlier has higher priorities. Priorities cannot be changed.

For command lines and environment, 
```
CommandLine:  --package.a.enabled=true
Environment: PACKAGE_A_ENABLED: false
```

## Performance

#### Load Performance

```log
benchmarking load/loadMock
time                 12.91 μs   (12.86 μs .. 12.95 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.95 μs   (12.90 μs .. 13.02 μs)
std dev              200.7 ns   (150.4 ns .. 267.6 ns)
variance introduced by outliers: 12% (moderately inflated)

benchmarking load/loadCMD
time                 666.5 ns   (665.6 ns .. 667.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 668.8 ns   (666.7 ns .. 672.7 ns)
std dev              9.395 ns   (5.114 ns .. 14.75 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking load/loadEnv
time                 1.823 ms   (1.813 ms .. 1.833 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.803 ms   (1.791 ms .. 1.814 ms)
std dev              39.13 μs   (31.88 μs .. 48.43 μs)

benchmarking load/loadYaml
time                 116.9 μs   (116.5 μs .. 117.3 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 118.6 μs   (116.6 μs .. 126.6 μs)
std dev              13.17 μs   (866.0 ns .. 27.98 μs)
variance introduced by outliers: 84% (severely inflated)

benchmarking load/loadToml
time                 2.801 ms   (2.763 ms .. 2.833 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 2.890 ms   (2.834 ms .. 3.056 ms)
std dev              266.0 μs   (45.31 μs .. 481.7 μs)
variance introduced by outliers: 63% (severely inflated)
```

#### Parse Performance

```log
benchmarking parse-int/int
time                 1.285 μs   (1.280 μs .. 1.291 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.294 μs   (1.289 μs .. 1.300 μs)
std dev              18.29 ns   (13.86 ns .. 26.49 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking parse-int/int/text
time                 593.9 ns   (592.4 ns .. 595.6 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 595.8 ns   (594.0 ns .. 598.8 ns)
std dev              7.536 ns   (5.243 ns .. 10.81 ns)
variance introduced by outliers: 12% (moderately inflated)

benchmarking parse-int/int/bool
time                 1.067 μs   (1.064 μs .. 1.070 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.068 μs   (1.065 μs .. 1.072 μs)
std dev              10.28 ns   (7.972 ns .. 13.55 ns)

benchmarking run/text
time                 953.9 ns   (951.3 ns .. 957.1 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 954.7 ns   (952.6 ns .. 958.4 ns)
std dev              8.659 ns   (5.963 ns .. 13.47 ns)

benchmarking run/bool
time                 1.218 μs   (1.214 μs .. 1.222 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.216 μs   (1.213 μs .. 1.221 μs)
std dev              13.03 ns   (9.765 ns .. 18.64 ns)
```

#### Parse IO Performance

```log
benchmarking parse-io/read-io
time                 7.803 ns   (7.785 ns .. 7.822 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.812 ns   (7.797 ns .. 7.838 ns)
std dev              68.51 ps   (41.76 ps .. 118.5 ps)

benchmarking parse-io/parse-io
time                 2.286 μs   (1.702 μs .. 3.317 μs)
                     0.494 R²   (0.456 R² .. 0.908 R²)
mean                 2.856 μs   (2.243 μs .. 3.872 μs)
std dev              2.663 μs   (1.837 μs .. 3.365 μs)
variance introduced by outliers: 99% (severely inflated)
```

As showed above, salak's performance is good enough for normal applications. Normally loading and parsing properties can be completed in less then 5ms. Also salak's `require` function support parsing `IO a` values, which can be used for dynamic values affected by reloading. Reading the wrapped `IO` value is much faster then `require`.

## Usage

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

instance FromProp m Config where
  fromProp = Config
    <$> "user" ? pattern "[a-z]{5,16}"
    <*> "pwd"
    <*> "ext" .?= 1

main = runSalakWith "salak" (YAML :|: TOML) $ do
  c :: Config <- require "test.config"
  lift $ print c
```

GHCi play
```Haskell
λ> :set -XFlexibleInstances -XMultiParamTypeClasses -XOverloadedStrings
λ> import Salak
λ> import Data.Default
λ> import Data.Text(Text)
λ> data Config = Config { name :: Text, dir  :: Maybe Text, ext  :: Int} deriving (Eq, Show)
λ> instance FromProp m Config where fromProp = Config <$> "user" <*> "dir" <*> "ext" .?= 1
λ> runSalak def (require "") :: IO Config
Config {name = "daniel", dir = Nothing, ext = 1}
```

TODO:
- Add git pull support.
- Add automatic reloading.
