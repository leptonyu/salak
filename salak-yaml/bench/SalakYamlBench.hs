module SalakYamlBench where

import           Criterion.Main
import           Salak
import           Salak.Yaml


main = defaultMain
    [ bgroup "load"
      [ bench "loadYaml" $ whnfIO $ loadAndRunSalak (loadYaml "salak.yml") action
      ]
    ]

action = return ()
