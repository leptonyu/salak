module Data.Salak.Environment where

import           Data.Char
import           Data.Salak.Types
import           Data.Text          (pack)
import           System.Environment

-- | Load `Properties` from 'Environment'
makePropertiesFromEnvironment :: Properties -> IO Properties
makePropertiesFromEnvironment p = getEnvironment >>= (\v -> return $ makePropertiesFromEnvironment' v p)


makePropertiesFromEnvironment' :: [(String,String)] -> Properties -> Properties
makePropertiesFromEnvironment' vs = makeProperties $ go <$> vs
  where
    go (k,v) = (pack $ fmap g2 k,PStr $ pack v)
    g2 '_' = '.'
    g2 a   = toLower a
