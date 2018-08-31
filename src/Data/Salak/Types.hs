{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Salak.Types where

import           Control.Monad       ((>=>))
import           Data.Char
import qualified Data.HashMap.Strict as M
import           Data.Int
import           Data.List.Split
import           Data.Maybe
import           Data.Scientific
import           Data.Text           (Text, pack, unpack)
import           Data.Word
import           Foreign.C.Types
import           Text.Read

-- | Property key
type Key = Text

-- | A Property value represented as a Haskell value.
data Property
  = PNum  !Scientific -- ^ Numeric Property
  | PStr  !String     -- ^ String  Property
  | PBool !Bool       -- ^ Bool    Property
  deriving (Eq, Show)

-- | A Property Container to hold all properties
data Properties
  = Properties [Property] [M.HashMap Key Properties]
  deriving (Eq)

instance Show Properties where
  show = unlines . go ""
    where
      {-# INLINE go #-}
      {-# INLINE g2 #-}
      {-# INLINE g3 #-}
      {-# INLINE g4 #-}
      go p (Properties ps ms) = fmap (g2 p) ps ++ concat (fmap (g3 p) ms)
      g2 p (PNum n)  = p ++ "=" ++ show n
      g2 p (PStr n)  = p ++ "=" ++ n
      g2 p (PBool n) = p ++ "=" ++ show n
      g3 :: String -> M.HashMap Key Properties -> [String]
      g3 p m = concat $ fmap (g4 p) $ M.toList m
      g4 "" (p2,ps) = go (unpack p2) ps
      g4 p  (p2,ps) = go (p ++ "." ++ unpack p2) ps

-- | The empty `Properties`
empty :: Properties
empty = Properties [] []

-- | Split origin key by '.' to sub keys:
--   
-- > "salak.config.name" -> ["salak","config","name"]
-- > "" -> []
-- > "a..b" -> ["a","b"]
-- 
toKeys :: String -> [Key]
toKeys = fmap pack . filter (not.null) . splitOneOf "."

-- | Insert simple `Property` into `Properties` by `Key`.
-- If the key already have values then the new property will discard.
insert :: [Key] -> Property -> Properties -> Properties
insert []     p (Properties [] m)  = Properties [p] m
insert []     _ (Properties ps m)  = Properties ps  m
insert (a:as) p (Properties ps []) = Properties ps [M.insert a (insert as p empty) M.empty]
insert (a:as) p (Properties ps ms) = Properties ps $ go a as p <$> ms
  where
    go a as p m = case M.lookup a m of
      Just n  -> M.insert a (insert as p     n) m
      Nothing -> M.insert a (insert as p empty) m

-- | Find `Properties` by key and convert to specific Haskell value.
-- Return `Nothing` means not found, and throw `ErrorCall` means convert failed.
lookup :: FromProperties a => String -> Properties -> Maybe a
lookup = go . toKeys
  where
    go []     p = from $ fromProperties p
    go (a:as) (Properties _ [m]) = case M.lookup a m of
      Just n  -> go as n
      Nothing -> Nothing
    go (a:as) _ = Nothing

-- | Insert batch properties to `Properties`
makeProperties :: [(String, Property)] -> Properties -> Properties
makeProperties ps m = foldl go m ps
  where
    go m (k,v) = insert (toKeys k) v m

-- | Return of `FromProperties`
data Return a
  = Empty
  | OK a
  | Fail String
  deriving Show

instance Functor Return where
  fmap f (OK a)   = OK (f a)
  fmap _ Empty    = Empty
  fmap _ (Fail b) = Fail b

instance Applicative Return where
  pure = OK
  (OK f) <*> (OK a) = OK (f a)
  (Fail x) <*> (Fail y) = Fail $ x ++ ";" ++ y
  (Fail x) <*> _ = Fail x
  _ <*> (Fail y) = Fail y
  _ <*> _ = Empty

instance Monad Return where
  (OK a)   >>= f = f a
  Empty    >>= _ = Empty
  (Fail b) >>= _ = Fail b

fromReturn :: b -> Return b -> b
fromReturn _ (OK a) = a
fromReturn a _      = a

mapReturn :: (a -> Return b) -> [a] -> [b]
mapReturn f as = go $ fmap f as
  where
    go []        = []
    go (OK a:as) = a : go as
    go (_:as)    = go as

-- | Convert `Properties` to Haskell value.
class FromProperties a where
  fromProperties :: Properties -> Return a

instance FromProperties Property where
  fromProperties (Properties (a:_) _) = OK a
  fromProperties _              = Empty

instance {-# OVERLAPPABLE #-} FromProperties a => FromProperties [a] where
  fromProperties (Properties ps ms) =
    let ns = fmap (\p -> Properties [p] []) ps ++ fmap (\m -> Properties [] [m]) ms
    in OK $ mapReturn fromProperties ns

instance FromProperties Scientific where
  fromProperties = fromProperties >=> go
    where
      go (PNum a) = OK a
      go (PStr a) = to readMaybe a
      go _        = Empty

instance FromProperties String where
  fromProperties = fromProperties >=> go
    where
      go (PStr a)  = OK a
      go (PNum a)  = OK $ show a
      go (PBool a) = OK $ toLower <$> show a

instance FromProperties Text where
  fromProperties a = pack <$> fromProperties a

instance FromProperties Float where
  fromProperties a = toRealFloat <$> fromProperties a

instance FromProperties Double where
  fromProperties a = toRealFloat <$> fromProperties a

instance FromProperties Int where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Int8 where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Int16 where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Int32 where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Int64 where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Word where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Word8 where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Word16 where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Word32 where
  fromProperties = fromProperties >=> to toBoundedInteger

instance FromProperties Word64 where
  fromProperties = fromProperties >=> to toBoundedInteger

to :: (b -> Maybe a) -> b -> Return a
to v a = case v a of
  Just a  -> OK a
  Nothing -> Fail "number convert failed"

from :: Return a -> Maybe a
from (OK a)   = Just a
from Empty    = Nothing
from (Fail e) = error e

instance FromProperties Bool where
  fromProperties = fromProperties >=> go
    where
      go (PBool a) = OK a
      go (PStr  a) = g2 $ fmap toLower a
      go _         = Fail "number cannot convert to bool"
      g2 "true"  = OK True
      g2 "false" = OK False
      g2 _       = Empty

instance FromProperties Char where
  fromProperties = fromProperties >=> go
    where
      go (PStr (a:_)) = OK a
      go _            = Fail "cannot convert to char"

instance FromProperties CTime where
  fromProperties a = CTime <$> fromProperties a

