{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Salak.Aeson where

import           Data.Aeson
import           Data.Char
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Salak.Property
import           Data.Text           (pack, unpack)
import           Data.Vector         (fromList, toList)

makePropertiesFromJson :: Value -> Properties -> Properties
makePropertiesFromJson Null       p = p
makePropertiesFromJson (Bool b)   p = insert [] (PBool b) p
makePropertiesFromJson (Number n) p = insert [] (PNum  n) p
makePropertiesFromJson (String s) p = insert [] (PStr  $ unpack s) p
makePropertiesFromJson (Array  v) (Node ps ms)  = let (nps,nms) = fromArray v in Node (ps++nps) (ms++nms)
makePropertiesFromJson (Object o) (Node ps [])  = Node ps [M.map jsonToProperties o]
makePropertiesFromJson (Object o) (Node ps [m]) = Node ps [m `M.union` M.map jsonToProperties o]
makePropertiesFromJson (Object o) p = p

jsonToProperties :: Value -> Properties
jsonToProperties = (`makePropertiesFromJson` empty)

fromArray v = foldl g3 ([],[]) $ go . jsonToProperties <$> toList v
  where
    go (Node ps ms) = (g2 ps,g2 ms)
    g2 []    = []
    g2 (a:_) = [a]
    g3 (as,bs) (a,b) = (as++a,bs++b)

instance FromProperties Value where
  fromProperties (Node []        []) = Empty
  fromProperties (Node [PBool p] []) = OK $ Bool p
  fromProperties (Node [PNum  p] []) = OK $ Number p
  fromProperties (Node [PStr  p] []) = OK $ String $ pack p
  fromProperties (Node ps [])        = OK $ Array $ fromList $ mapReturn (fromProperties.(\p-> Node [p] [])) ps
  fromProperties (Node _ [m])        = OK $ Object $ M.map (fromReturn Null . fromProperties) m
  fromProperties (Node _  ms)        = OK $ Array $ fromList $ mapReturn (fromProperties.(\m-> Node [] [m])) ms

instance {-# OVERLAPPABLE #-} FromJSON a => FromProperties a where
  fromProperties a = do
    v :: Value <- fromProperties a
    case fromJSON v of
      Success r -> OK r
      Error   e -> Fail e
