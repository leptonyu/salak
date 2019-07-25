{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Salak.Trie(
    Trie
  , getPrimitive
  , getMap
  -- * Construction
  , empty
  , singleton
  -- * Basic interface
  , Salak.Trie.null
  , member
  , lookup
  , insert
  , modify
  , modifyF
  , modify'
  , update
  , alter
  , alterF
  -- * Lists
  , Salak.Trie.toList
  , fromList
  -- * Filter
  , filter
  -- * Combine
  , unionWith
  , unionWith'
  ) where

import           Control.Applicative (pure, (<*>))
import           Control.Monad       (Monad (..))
import           Data.Bool
import           Data.Eq
import           Data.Foldable       (Foldable (..))
import           Data.Function
import           Data.Functor
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (concat, intercalate, map, reverse, (++))
import           Data.Maybe
import           Data.Traversable
import           Data.Tuple          (uncurry)
import           Salak.Internal.Key
import           Text.Show           (Show (..))

-- | A Trie from keys `Key` to values @v@, which is a recursive map.
data Trie v = Trie !(Maybe v) !(HashMap Key (Trie v)) deriving (Eq, Functor)

instance Show v => Show (Trie v) where
  show t = intercalate "\n" $ map (\(k,v)-> show k ++ ":" ++ show v) $ Salak.Trie.toList t

instance Foldable Trie where
  foldr f b (Trie v m) = foldr (flip (foldr f)) (go v) m
    where
      go (Just x) = f x b
      go _        = b

instance Traversable Trie where
  traverse f (Trie v m) = Trie <$> go v <*> traverse (traverse f) m
    where
      go (Just x) = Just <$> (f x)
      go _        = pure Nothing

-- | /O(1)/. A trie with a single element.
singleton :: v -> Trie v
singleton v = Trie (Just v) HM.empty

-- | /O(1)/. The empty trie.
empty :: Trie v
empty = Trie Nothing HM.empty

-- | Get primitive value of a trie.
getPrimitive :: Trie v -> Maybe v
getPrimitive (Trie v _) = v

-- | Get map value of a trie.
getMap :: Trie v -> HashMap Key (Trie v)
getMap (Trie _ m) = m

-- | /O(1)/. Return True if this trie is empty, False otherwise.
null :: Trie v -> Bool
null (Trie Nothing e) = HM.null e
null _                = False

-- | /O(log (n+m))/. Return True if the specified key is present in the trie, False otherwise.
member :: Eq v => Keys -> Trie v -> Bool
member k t = isJust (lookup k t)

-- | /O(log (n+m))/. Return the primitive value to which the specified key is mapped,
-- or Nothing if this trie contains no mapping for the key.
lookup :: Eq v => Keys -> Trie v -> Maybe v
lookup (Keys keys) = go keys
  where
    go []     (Trie v _) = v
    go (k:ks) (Trie _ m) = case HM.lookup k m of
      Just t -> go ks t
      _      -> Nothing

-- | /O(log n)/. Associate the specified value with the specified key in this trie.
-- If this trie previously contained a mapping for the key, the old value is replaced.
insert :: Eq v => Keys -> v -> Trie v -> Trie v
insert ks v = alter (\_ -> Just v) ks

-- | /O(log m)/. The expression (`modify` k f trie) modifies the sub trie at k.
modify :: Eq v => Key -> (Trie v -> Trie v) -> Trie v -> Trie v
modify k f (Trie v m) = Trie v $ HM.alter (go . f . fromMaybe empty) k m
  where
    go x = if x == empty then Nothing else Just x

-- | /O(log (n+m))/. The expression (`modify'` ks f trie) modifies the sub trie at ks.
modify' :: Eq v => Keys -> (Trie v -> Trie v) -> Trie v -> Trie v
modify' (Keys ks) f = foldr modify f ks

-- | /O(log m)/. The expression (`modifyF` k f trie) modifies the sub trie at k.
modifyF :: (Monad m, Eq v) => Key -> (Trie v -> m (Trie v)) -> Trie v -> m (Trie v)
modifyF k f (Trie v m) = Trie v <$> HM.alterF go k m
  where
    go (Just w) = (\x -> if x == empty then Nothing else Just x) <$> f w
    go _        = return Nothing

-- | /O(1)/. The expression (update f trie) updates the primitive value in the trie.
update :: Eq v => (Maybe v -> Maybe v) -> Trie v -> Trie v
update f = alter f (Keys [])

-- | /O(n)/. The expression (update f ks trie) updates the primitive value of sub trie at ks.
alter :: Eq v => (Maybe v -> Maybe v) -> Keys -> Trie v -> Trie v
alter f (Keys keys) = go keys
  where
    go []     (Trie v m) = Trie (f v) m
    go (k:ks) (Trie v m) = Trie v $ HM.alter (g2 ks) k m
    g2 ks t = let t1 = go ks (fromMaybe empty t) in if t1 == empty then Nothing else Just t1

-- | /O(n)/. The expression (update f ks trie) updates the primitive value of sub trie at ks.
alterF :: (Functor f, Eq v) => (Maybe v -> f(Maybe v)) -> Keys -> Trie v -> f (Trie v)
alterF f (Keys keys) = go keys
  where
    go []     (Trie v m) = (`Trie` m) <$> f v
    go (k:ks) (Trie v m) = Trie v <$> HM.alterF (g2 ks) k m
    g2 ks t = g3 <$> go ks (fromMaybe empty t)
    g3 t = if t == empty then Nothing else Just t

-- | /O(n*m)/. Return a list of this tries's elements. The list is produced lazily.
toList :: Trie v -> [(Keys, v)]
toList = go []
  where
    go p (Trie (Just v) m) = (Keys $ reverse p, v) : g2 p m
    go p (Trie _        m) = g2 p m
    g2 p m = concat $ g3 p <$> HM.toList m
    g3 p (k,t) = go (k:p) t

-- | /O(n*m*log n)/. Construct a trie with the supplied mappings.
-- If the list contains duplicate mappings, the later mappings take precedence.
fromList :: Eq v => [(Keys, v)] -> Trie v
fromList = foldr (uncurry insert) empty

-- | /O(n)/. Filter this trie by retaining only elements which values satisfy a predicate.
filter :: Eq v => (v -> Bool) -> Trie v -> Trie v
filter f (Trie v m) = if ok v then Trie v go else Trie Nothing go
  where
    ok (Just x) = f x
    ok _        = False
    go = HM.mapMaybe (g2 . filter f) m
    g2 x = if x == empty then Nothing else Just x

-- | /O(n+m)/. The union of two tries.
-- If a key occurs in both tries, the provided function (first argument) will be used to compute the result.
unionWith :: Eq v => (Maybe v -> Maybe v -> Maybe v) -> Trie v -> Trie v -> Trie v
unionWith f (Trie v1 m1) (Trie v2 m2) = Trie (f v1 v2) $ HM.unionWith (unionWith f) m1 m2

-- | /O(n+m)/. The union of two tries.
-- All the keys will be calculated by the provided function.
unionWith' :: (Maybe v -> Maybe v -> Maybe v3) -> Trie v -> Trie v -> Trie v3
unionWith' f (Trie v1 m1) (Trie v2 m2) = Trie (f v1 v2) $ foldr go HM.empty $ HM.keys $ HM.union m1 m2
  where
    go k =
      let x1 = fromMaybe empty $ HM.lookup k m1
          x2 = fromMaybe empty $ HM.lookup k m2
      in HM.insert k (unionWith' f x1 x2)
