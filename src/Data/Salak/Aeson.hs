{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Salak.Aeson where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Salak.Types
import           Data.Vector         (fromList, toList)

-- | Load `Properties` from JSON `Value`
makePropertiesFromJson :: Value -> Properties -> Properties
makePropertiesFromJson Null       p = p
makePropertiesFromJson (Bool b)   p = insert [] (PBool b) p
makePropertiesFromJson (Number n) p = insert [] (PNum  n) p
makePropertiesFromJson (String s) p = insert [] (PStr  s) p
makePropertiesFromJson (Array  v) (Properties ps ms)  = let (nps,nms) = fromArray v in Properties (ps++nps) (ms++nms)
makePropertiesFromJson (Object o) (Properties ps [])  = Properties ps [M.map jsonToProperties o]
makePropertiesFromJson (Object o) (Properties ps [m]) = Properties ps [m `M.union` M.map jsonToProperties o]
makePropertiesFromJson (Object _) p = p

jsonToProperties :: Value -> Properties
jsonToProperties = (`makePropertiesFromJson` empty)

fromArray v = foldl g3 ([],[]) $ go . jsonToProperties <$> toList v
  where
    go (Properties ps ms) = (g2 ps,g2 ms)
    g2 []    = []
    g2 (a:_) = [a]
    g3 (as,bs) (a,b) = (as++a,bs++b)

instance FromProperties Value where
  fromProperties (Properties []        []) = Empty
  fromProperties (Properties [PBool p] []) = OK $ Bool   p
  fromProperties (Properties [PNum  p] []) = OK $ Number p
  fromProperties (Properties [PStr  p] []) = OK $ String p
  fromProperties (Properties ps [])        = Array . fromList <$> traverse (fromProperties.singleton) ps
  fromProperties (Properties _ [m])        = OK $ Object $ M.map (fromReturn Null . fromProperties) m
  fromProperties (Properties _  ms)        = Array . fromList <$> traverse (fromProperties.singletonMap) ms

instance {-# OVERLAPPABLE #-} FromJSON a => FromProperties a where
  fromProperties a = do
    v :: Value <- fromProperties a
    case fromJSON v of
      Success r -> OK r
      Error   e -> Fail e
