{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Salak.Types where

import           Control.Monad       ((>=>))
import           Data.Char
import qualified Data.HashMap.Strict as M
import           Data.Int
import           Data.Maybe
import           Data.Scientific
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Word
import           Text.Read

-- | Property key
type Key = Text

-- | A Property value represented as a Haskell value.
data Property
  = PNum  !Scientific -- ^ Numeric Property
  | PStr  !Text       -- ^ String  Property
  | PBool !Bool       -- ^ Bool    Property
  deriving Eq

instance Show Property where
  {-# INLINE show #-}
  show (PNum  n) = show n
  show (PStr  n) = T.unpack n
  show (PBool n) = toLower <$> show n

instance IsString Property where
  fromString = PStr . T.pack

-- | A Property Container to hold all properties
data Properties
  = Properties [Property] [M.HashMap Key Properties]
  deriving (Eq)

instance Show Properties where
  {-# INLINE show #-}
  show = unlines . go ""
    where
      {-# INLINE go #-}
      {-# INLINE g2 #-}
      {-# INLINE g3 #-}
      {-# INLINE g4 #-}
      {-# INLINE convert #-}
      go p (Properties ps ms) = convert p g2 ps ++ concat (convert p g3 ms)
      g2 "" a = ".=" ++ show a
      g2 p  a = p ++ "=" ++ show a
      g3 :: String -> M.HashMap Key Properties -> [String]
      g3 p = concatMap (g4 p) . M.toList
      g4 "" (p2,ps) = go (T.unpack p2) ps
      g4 p  (p2,ps) = go (p ++ "." ++ T.unpack p2) ps
      convert _ _ []  = []
      convert p f [a] = [f p a]
      convert p f as  = zipWith (\(i :: Int) a -> f (p ++ "[" ++ show i ++ "]") a) [0..] as

-- | The empty `Properties`
empty :: Properties
empty = Properties [] []


singleton :: Property -> Properties
singleton p = Properties [p] []

singletonMap :: M.HashMap Key Properties -> Properties
singletonMap m = Properties [] [m]

-- | Split origin key by '.' to sub keys:
--
-- > "salak.config.name" -> ["salak","config","name"]
-- > "" -> []
-- > "a..b" -> ["a","b"]
--
toKeys :: Text -> [Key]
toKeys = filter (not.T.null) . T.splitOn "."

-- | Insert simple `Property` into `Properties` by `Key`.
-- If the key already have values then the new property will discard.
insert :: [Key] -> Property -> Properties -> Properties
insert []     p (Properties [] m)  = Properties [p] m
insert []     _ (Properties ps m)  = Properties ps  m
insert (a:as) p (Properties ps []) = Properties ps  [insertMap as p a M.empty]
insert (a:as) p (Properties ps ms) = Properties ps $ insertMap as p a <$> ms

insertMap as p = M.alter (Just . insert as p . fromMaybe empty)

-- | Find `Properties` by key and convert to specific Haskell value.
-- Return `Nothing` means not found, and throw `ErrorCall` means convert failed.
lookup :: FromProperties a => Text -> Properties -> Maybe a
lookup k = from . lookup' k
  where
    from :: Return a -> Maybe a
    from (Right a)           = Just a
    from (Left (EmptyKey _)) = Nothing
    from (Left (Fail     e)) = error e

-- | Find `Properties` by key and convert to specific Haskell value.
lookup' :: FromProperties a => Text -> Properties -> Return a
lookup' k = go (toKeys k)
  where
    go [] p                      = fromProperties p
    go (a:as) (Properties _ [m]) = case M.lookup a m of
      Just n -> case go as n of
        Left (EmptyKey ke) -> Left $ EmptyKey $ joinKey a ke
        v                  -> v
      _      -> Left $ EmptyKey $ T.intercalate "." (a:as)
    go ke _                      = Left $ EmptyKey $ T.intercalate "." ke


joinKey :: Text -> Text -> Text
joinKey "" k = k
joinKey k "" = k
joinKey a b  = a <> "." <> b

-- | Insert batch properties to `Properties`
makeProperties :: [(Text, Property)] -> Properties -> Properties
makeProperties = flip (foldl go)
  where
    go m (k,v) = insert (toKeys k) v m

-- | Return of `FromProperties`
type Return = Either ErrResult
data ErrResult = EmptyKey Text | Fail String deriving Show

-- | Convert `Properties` to Haskell value.
class FromProperties a where
  fromProperties :: Properties -> Return a

instance FromProperties Property where
  fromProperties (Properties [a] _) = Right a
  fromProperties (Properties [] _)  = Left $ EmptyKey ""
  fromProperties _                  = Left $ Fail "property has multi values"

instance {-# OVERLAPPABLE #-} FromProperties a => FromProperties [a] where
  fromProperties (Properties ps ms) = traverse fromProperties $ fmap singleton ps <> fmap singletonMap ms

instance FromProperties Scientific where
  fromProperties = fromProperties >=> go
    where
      go (PNum a) = Right a
      go (PStr a) = to readMaybe $ T.unpack a
      go _        = Left $ Fail "bool cannot convert to number"

instance FromProperties String where
  fromProperties a = T.unpack <$> fromProperties a

instance FromProperties Text where
  fromProperties = fromProperties >=> go
    where
      go (PStr a) = Right a
      go a        = Right $ T.pack $ show a

instance FromProperties Float where
  fromProperties a = toRealFloat <$> fromProperties a

instance FromProperties Double where
  fromProperties a = toRealFloat <$> fromProperties a

instance FromProperties Int where
  fromProperties = toNumeric

instance FromProperties Int8 where
  fromProperties = toNumeric

instance FromProperties Int16 where
  fromProperties = toNumeric

instance FromProperties Int32 where
  fromProperties = toNumeric

instance FromProperties Int64 where
  fromProperties = toNumeric

instance FromProperties Word where
  fromProperties = toNumeric

instance FromProperties Word8 where
  fromProperties = toNumeric

instance FromProperties Word16 where
  fromProperties = toNumeric

instance FromProperties Word32 where
  fromProperties = toNumeric

instance FromProperties Word64 where
  fromProperties = toNumeric

toNumeric :: (Bounded i, Integral i) => Properties -> Return i
toNumeric = fromProperties >=> to toBoundedInteger

to :: (b -> Maybe a) -> b -> Return a
to v b = case v b of
  Just a  -> Right a
  Nothing -> Left $ Fail "number convert failed"

instance FromProperties Bool where
  fromProperties = fromProperties >=> go
    where
      go (PBool a) = Right a
      go (PStr  a) = g2 $ T.toLower a
      go _         = Left $ Fail "number cannot convert to bool"
      g2 :: Text -> Return Bool
      g2 "1"     = Right True
      g2 "true"  = Right True
      g2 "0"     = Right False
      g2 "false" = Right False
      g2 _       = Left $ Fail "string value cannot convert to bool"

instance FromProperties Char where
  fromProperties = fromProperties >=> go
    where
      go (PStr s)
        | T.null s  = Left  $ EmptyKey ""
        | otherwise = Right $ T.head s
      go _          = Left  $ Fail "cannot convert to char"

