{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Salak.Internal.Val where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.ByteString      (ByteString)
import           Data.Heap            (Heap)
import qualified Data.Heap            as H
import           Data.Int
import           Data.List            (intercalate)
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Time
import           Salak.Internal.Key


data Val v = Val !Int !v deriving (Eq, Show)

data ModType
  = Add
  | Mod
  | Del
  | Noop deriving (Eq, Show)

type Priority = Int

priority :: Val v -> Int
priority (Val i _) = i

data VRef
  = VRT !Text
  | VRR !Keys ![VRef]
  deriving Eq

instance Show VRef where
  show (VRT t)  = T.unpack t
  show (VRR k m) = "${" <> show k <> (if null m then go m else ":" <> go m) <> "}"
    where
      go []     = ""
      go (x:as) = show x <> go as

data Value
  = VT  !Text
  | VI  !Scientific
  | VB  !Bool
  | VLT !LocalTime
  | VD  !Day
  | VH  !TimeOfDay
  | VZT !TimeZone !LocalTime
  | VU  !UTCTime
  | VR  ![VRef]
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
typeOfV (VU  b)   = ("UTCTime",   show b)
typeOfV (VR  b)   = ("Ref",       show b)

getType :: Value -> String
getType = fst . typeOfV

mkValue :: Value -> Either String Value
mkValue (VT v) = case parseOnly (vref <* endOfInput) v of
  Left  e       -> Left e
  Right x       -> Right $ case go x of
    [VRT _] -> VT v
    vs      -> VR vs
  where
    go (VRT a:VRT b:as) = go (VRT (a <> b):as)
    go (VRT a:b:as)     = VRT a:b:go as
    go (a:as)           = a : go as
    go []               = []
mkValue v      = Right v

-- mkValue' :: Value -> Value
-- mkValue' v = case mkValue v of
--   Left  _ -> v
--   Right x -> x

exprChar :: Parser Char
exprChar = satisfy (notInClass "\\${}") <|> go
  where
    go = do
      a <- char '\\'
      v <- peekChar
      case v of
        Just '\\' -> char '\\'
        Just '$'  -> char '$'
        Just '{'  -> char '{'
        Just '}'  -> char '}'
        Just x    -> fail $ "error char sequence \\" <> [x]
        _         -> return a

vref :: Parser [VRef]
vref = many1' (go <|> (VRT . T.pack <$> many1' exprChar))
  where
    go = do
      _ <- string "${"
      k <- exprs
      v <- option [] $ (char ':' >> option [VRT ""] vref )
      _ <- char '}'
      return (VRR (fromKeys k) v)

newtype Vals = Vals { unVals :: Heap (Val Value) } deriving Eq

instance Show Vals where
  show (Vals v) = intercalate "," $ go <$> H.toUnsortedList v
    where
      go (Val i x) = '#' : show i ++ ('.' : show x)

instance Eq v => Ord (Val v) where
  compare (Val a _) (Val b _) = compare a b

nullVals :: Vals -> Bool
nullVals (Vals v) = H.null v

minimumVals :: Vals -> Val Value
minimumVals (Vals h) = H.minimum h

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

modVals :: Val Value -> Vals -> Either String Vals
modVals (Val p x) (Vals v) = case mkValue x of
  Left  e -> Left e
  Right y -> Right $ Vals $ H.insert (Val p y) $ H.filter ((/=p) . priority) v

singletonVals :: Val Value -> Either String Vals
singletonVals (Val p x) = case mkValue x of
  Left  e -> Left e
  Right y -> Right $ Vals $ H.singleton $ Val p y

modVals' :: Vals -> Vals -> Either String Vals
modVals' (Vals v) vals = if H.null v then Right vals else modVals (H.minimum v) vals






