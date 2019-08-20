module Main where

import           Criterion.Main
import           Data.Default
import           Salak


main = defaultMain
  [ bgroup "load"
    [ bench "loadMock" $ whnfIO $ loadAndRunSalak (loadMock vals) action
    , bench "loadCMD"  $ whnfIO $ loadAndRunSalak (loadCommandLine def) action
    , bench "loadEnv"  $ whnfIO $ loadAndRunSalak loadEnv action
    ]
  , bgroup "run"
    [ bench "int" $ whnfIO $ (run $ require "int" :: IO Int)
    ]
  ]


action = return ()

run = loadAndRunSalak (loadMock vals)


vals =
  [ ("int",  "128")
  , ("a.b.c.d.e.f.g.h.i.j.k[0][0].text", "xxx")
  , ("a.b.c.d.e.f.g.h.i.j.k[0][0].bool", "true")
  , ("empty", "")
  ]
