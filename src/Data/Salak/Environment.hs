module Data.Salak.Environment where

import           Data.Char
import           Data.Salak.Property
import           System.Environment

makePropertiesFromEnvironment :: Properties -> IO Properties
makePropertiesFromEnvironment p = getEnvironment >>= (\v -> return $ makePropertiesFromEnvironment' v p)


makePropertiesFromEnvironment' :: [(String,String)] -> Properties -> Properties
makePropertiesFromEnvironment' vs = makeProperties $ go <$> vs
  where
    go (k,v) = (fmap g2 k,PStr v)
    g2 '_' = '.'
    g2 a   = toLower a
