{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Salak.Internal.Val where

import           Data.ByteString    (ByteString)
import           Data.Heap          (Heap)
import qualified Data.Heap          as H
import           Data.Int
import           Data.List          (intercalate)
import           Data.Scientific
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time


data Val v = Val !Int !v deriving (Eq, Show)
data ModType
  = Add
  | Mod
  | Del
  | Noop deriving (Eq, Show)

type Priority = Int

priority :: Val v -> Int
priority (Val i _) = i

data Value
  = VT  !Text
  | VI  !Scientific
  | VB  !Bool
  | VLT !LocalTime
  | VD  !Day
  | VH  !TimeOfDay
  | VZT !TimeZone !LocalTime
  | VU  !UTCTime
  deriving Eq

instance Show Value where
  show v = let (a,b) = typeOfV v in b ++ "::" ++ a


typeOfV :: Value -> (String, String)
typeOfV (VT  b)   = ("Str",       show b)
typeOfV (VI  b)   = ("Num",       show b)
typeOfV (VB  b)   = ("Bool",      show b)
typeOfV (VLT b)   = ("LocalTime", show b)
typeOfV (VD  b)   = ("Day",       show b)
typeOfV (VH  b)   = ("TimeOfDay", show b)
typeOfV (VZT _ b) = ("ZonedTime", show b)
typeOfV (VU b)    = ("UTCTime",   show b)

getType :: Value -> String
getType = fst . typeOfV

newtype Vals = Vals { unVals :: Heap (Val Value) } deriving Eq

instance Show Vals where
  show (Vals v) = intercalate "," $ go <$> H.toUnsortedList v
    where
      go (Val i x) = '#' : show i ++ ('.' : show x)

instance Eq v => Ord (Val v) where
  compare (Val a _) (Val b _) = compare a b

nullVals :: Vals -> Bool
nullVals (Vals v) = H.null v

emptyVals :: Vals
emptyVals = Vals H.empty

deleteVals :: Int -> Vals -> (Bool, Vals)
deleteVals i (Vals v) =
  let (a,b) = H.partition ((==i) . priority) v
  in (H.null a, Vals b)

getVal :: Vals -> Maybe Value
getVal (Vals v)
  | H.null v = Nothing
  | otherwise = let Val _ x = H.minimum v in Just x

class ToValue a where
  toVal :: a -> Value

instance ToValue Value where
  toVal = id

instance ToValue Text where
  toVal = VT

instance ToValue ByteString where
  toVal = VT . decodeUtf8

instance ToValue String where
  toVal = VT . T.pack

instance ToValue Scientific where
  toVal = VI

instance ToValue Integer where
  toVal = VI . fromInteger

instance ToValue Int where
  toVal = VI . fromInteger . toInteger

instance ToValue Int64 where
  toVal = VI . fromInteger . toInteger

instance ToValue Double where
  toVal = VI . realToFrac

instance ToValue Bool where
  toVal = VB

instance ToValue UTCTime where
  toVal = VU

delVals :: Int -> Vals -> Vals
delVals p (Vals v) = Vals $ H.filter ((/=p) . priority) v

modVals :: Val Value -> Vals -> Vals
modVals val@(Val p _) (Vals v) = Vals $ H.insert val $ H.filter ((/=p) . priority) v


modVals' :: Vals -> Vals -> Vals
modVals' (Vals v) vals = if H.null v then vals else modVals (H.minimum v) vals






