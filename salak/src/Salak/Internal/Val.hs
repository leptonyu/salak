module Salak.Internal.Val where

import           Control.Applicative  ((<|>))
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
import           Text.Megaparsec
import           Text.Megaparsec.Char
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup       ((<>))
#endif


data Val v = Val !Int !v deriving (Eq, Show)

data ModType
  = Add
  | Mod
  | Del
  | Noop deriving (Eq, Show)

type Priority = Int

{-# INLINE priority #-}
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
      {-# INLINE go #-}
      go = foldr ((<>) . show) ""

data Value
  = VT  !Text
  | VI  !Scientific
  | VB  !Bool
  | VLT !LocalTime
  | VD  !Day
  | VH  !TimeOfDay
  | VU  !UTCTime
  | VR  ![VRef]
  deriving Eq

{-# INLINE nullValue #-}
nullValue :: Value -> Bool
nullValue (VT x)  = T.null x
nullValue (VR []) = True
nullValue _       = False

instance Show Value where
  show v = let (a,b) = typeOfV v in b ++ "::" ++ a

{-# INLINE typeOfV #-}
typeOfV :: Value -> (String, String)
typeOfV (VT  b) = ("Str",       show b)
typeOfV (VI  b) = ("Num",       show b)
typeOfV (VB  b) = ("Bool",      show b)
typeOfV (VLT b) = ("LocalTime", show b)
typeOfV (VD  b) = ("Day",       show b)
typeOfV (VH  b) = ("TimeOfDay", show b)
typeOfV (VU  b) = ("UTCTime",   show b)
typeOfV (VR  b) = ("Ref",       show b)

{-# INLINE getType #-}
getType :: Value -> String
getType = fst . typeOfV

{-# INLINE mkValue #-}
mkValue :: Value -> Either String Value
mkValue (VT v) = if T.null v
  then Right (VT v)
  else case parse vref "" v of
    Left  e   -> Left (errorBundlePretty e)
    Right y   -> Right $ case y of
      [VRT x] -> VT x
      vs      -> VR vs
mkValue v      = Right v

{-# INLINE exprChar #-}
exprChar :: Parser Char
exprChar = noneOf go <|> (char '\\' >> oneOf go)
  where
    go :: [Token Text]
    go = "${}\\"

vref :: Parser [VRef]
vref = some (go <|> (VRT . T.pack <$> some exprChar))
  where
    {-# INLINE go #-}
    go = do
      _ <- string "${"
      k <- keyExpr
      v <- option [] $ char ':' >> option [VRT ""] vref
      _ <- char '}'
      return (VRR (fromKeys k) v)

newtype Vals = Vals { unVals :: Heap (Val Value) } deriving Eq

instance Show Vals where
  show (Vals v) = intercalate "," $ go <$> H.toUnsortedList v
    where
      go (Val i x) = '#' : show i ++ ('.' : show x)

instance Eq v => Ord (Val v) where
  compare (Val a _) (Val b _) = compare a b

{-# INLINE nullVals #-}
nullVals :: Vals -> Bool
nullVals (Vals v) = H.null v

{-# INLINE minimumVals #-}
minimumVals :: Vals -> Val Value
minimumVals (Vals h) = H.minimum h

{-# INLINE emptyVals #-}
emptyVals :: Vals
emptyVals = Vals H.empty

{-# INLINE deleteVals #-}
deleteVals :: Int -> Vals -> (Bool, Vals)
deleteVals i (Vals v) =
  let (a,b) = H.partition ((==i) . priority) v
  in (H.null a, Vals b)

{-# INLINE getVal #-}
getVal :: Vals -> Maybe Value
getVal (Vals v)
  | H.null v = Nothing
  | otherwise = let Val _ x = H.minimum v in Just x

class ToValue a where
  toVal :: a -> Value

instance ToValue Value where
  {-# INLINE toVal #-}
  toVal = id

instance ToValue Text where
  {-# INLINE toVal #-}
  toVal = VT

instance ToValue ByteString where
  {-# INLINE toVal #-}
  toVal = VT . decodeUtf8

instance ToValue String where
  {-# INLINE toVal #-}
  toVal = VT . T.pack

instance ToValue Scientific where
  {-# INLINE toVal #-}
  toVal = VI

instance ToValue Integer where
  {-# INLINE toVal #-}
  toVal = VI . fromInteger

instance ToValue Int where
  {-# INLINE toVal #-}
  toVal = VI . fromInteger . toInteger

instance ToValue Int64 where
  {-# INLINE toVal #-}
  toVal = VI . fromInteger . toInteger

instance ToValue Double where
  {-# INLINE toVal #-}
  toVal = VI . realToFrac

instance ToValue Bool where
  {-# INLINE toVal #-}
  toVal = VB

instance ToValue UTCTime where
  {-# INLINE toVal #-}
  toVal = VU

{-# INLINE delVals #-}
delVals :: Int -> Vals -> Vals
delVals p (Vals v) = Vals $ H.filter ((/=p) . priority) v

modVals :: Val Value -> Vals -> Either String Vals
modVals (Val p x) (Vals v) = case mkValue x of
  Left  e -> Left e
  Right y -> Right $ Vals $ H.insert (Val p y) $ H.filter ((/=p) . priority) v

{-# INLINE singletonVals #-}
singletonVals :: Val Value -> Either String Vals
singletonVals (Val p x) = case mkValue x of
  Left  e -> Left e
  Right y -> Right $ Vals $ H.singleton $ Val p y

modVals' :: Vals -> Vals -> Either String Vals
modVals' (Vals v) vals = if H.null v then Right vals else modVals (H.minimum v) vals






