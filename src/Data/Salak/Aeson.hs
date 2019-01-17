{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Salak.Aeson where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict    as M
import           Data.Menshen
import           Data.Salak.Types
import           Data.Vector            (toList)

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

instance HasValid Parser where
  invalid = fail . toI18n

fromArray v = foldl g3 ([],[]) $ go . jsonToProperties <$> toList v
  where
    go (Properties ps ms) = (g2 ps,g2 ms)
    g2 []    = []
    g2 (a:_) = [a]
    g3 (as,bs) (a,b) = (as++a,bs++b)

-- | Load Properties from JSON Value
--
-- @since 0.2.2
loadJSON :: MonadIO m => Value -> LoadProperties m ()
loadJSON = modify . makePropertiesFromJson
