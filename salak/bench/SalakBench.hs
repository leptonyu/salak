module Main(main) where

import           Control.Monad.Reader
import           Criterion.Main
import           Data.Default
import           Data.Text              (Text)
import           Data.Time
import           Data.Time.Clock.System
import           Salak


main = do
  sp <- loadAndRunSalak' (loadMock vals) return
  let run :: forall a. ReaderT SourcePack IO a -> IO a
      run = flip runReaderT sp
  v  <- run $ require "int" :: IO (IO Int)
  defaultMain
    [ bgroup "load"
      [ bench "loadMock" $ whnfIO $ loadAndRunSalak (loadMock vals) action
      , bench "loadCMD"  $ whnfIO $ loadAndRunSalak (loadCommandLine def) action
      , bench "loadEnv"  $ whnfIO $ loadAndRunSalak loadEnv action
      ]
    , bgroup "parse-io"
      [ bench "read-io"  $ whnfIO v
      , bench "parse"    $ whnfIO (run $ require "int" :: IO (IO Int))
      ]
    , bgroup "parse-int"
      [ bench "int"      $ whnfIO (run $ require "int" :: IO Int)
      , bench "int/text" $ whnfIO (run $ require "int" :: IO Text)
      , bench "int/bool" $ whnfIO (run $ require "int" :: IO (Either String Bool))
      ]
    , bgroup "run"
      [ bench "text"     $ whnfIO $ (run $ require "a.b.c.d.e.f.g.h.i.j.k.text" :: IO Text)
      , bench "bool"     $ whnfIO $ (run $ require "a.b.c.d.e.f.g.h.i.j.k.bool" :: IO Bool)
      ]
    , bgroup "gettime"
      [ bench "current"  $ whnfIO getCurrentTime
      , bench "system"   $ whnfIO getSystemTime
      ]
    ]

action = return ()

vals =
  [ ("int",  "128")
  , ("a.b.c.d.e.f.g.h.i.j.k.text", "xxx")
  , ("a.b.c.d.e.f.g.h.i.j.k.bool", "true")
  , ("empty", "")
  ]
