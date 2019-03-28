# salak

[![Hackage](https://img.shields.io/hackage/v/salak.svg)](https://hackage.haskell.org/package/salak)
[![stackage LTS package](http://stackage.org/package/salak/badge/lts)](http://stackage.org/lts/package/salak)
[![stackage Nightly package](http://stackage.org/package/salak/badge/nightly)](http://stackage.org/nightly/package/salak)
[![Build Status](https://travis-ci.org/leptonyu/salak.svg?branch=master)](https://travis-ci.org/leptonyu/salak)


Configuration Loader for Production in Haskell.

This library default a standard configuration load process. It can load properties from `CommandLine`, `Environment`,
`JSON value` and `Yaml` files. They all load to the same format `SourcePack`. Earler property source has higher order
to load property. For example:

```
CommandLine:  --package.a.enabled=true
Environment: PACKAGE_A_ENABLED: false

lookup "package.a.enabled" properties => Just True
```

`CommandLine` has higher order then `Environment`, for the former load properties earler then later.

Usage:

```Haskell
data Config = Config
  { name :: Text
  , dir  :: Maybe Text
  , ext  :: Int
  } deriving (Eq, Show)

instance FromProp Config where
  fromProp = Config
    <$> "user"
    <*> "pwd"
    <*> "ext" .?= 1

main = do
  c :: Config <- defaultLoadSalak def $ require ""
  print c
```

```
λ> c
Config {name = "daniel", dir = Nothing, ext = 1}
```