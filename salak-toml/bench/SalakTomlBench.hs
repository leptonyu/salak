module SalakTomlBench where

import           Criterion.Main
import           Salak
import           Salak.Toml


main = defaultMain
    [ bgroup "load"
      [ bench "loadYaml" $ whnfIO $ loadAndRunSalak (loadToml "salak.toml") action
      ]
    ]

action = return ()
