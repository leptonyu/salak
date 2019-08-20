{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Salak.Internal
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- A data structure for manipulating properties.
--
module Salak.Trie(
    Trie(..)
  -- * Construction
  , empty
  , singleton
  -- * Basic interface
  , Salak.Trie.null
  , member
  , lookup
  , subTrie
  , subTries
  , insert
  , modify
  -- , modifyF
  , modify'
  , update
  , alter
  -- , alterF
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
import           Data.Bool
import qualified Data.DList          as D
import           Data.Eq
import           Data.Foldable       (Foldable (..))
import           Data.Function
import           Data.Functor
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List           (concat, intercalate, map, (++))
import           Data.Maybe
import           Data.Traversable
import           Data.Tuple          (uncurry)
import           Salak.Internal.Key
import           Text.Show           (Show (..))

-- | A Trie from keys `Key` to values @v@, which is a recursive map.
data Trie v = Trie
  { tvar :: !(Maybe v)
  , tmap :: !(HashMap Key (Trie v))
  } deriving (Eq, Functor)

instance Show v => Show (Trie v) where
  show t = intercalate "\n" $ map (\(k,v)-> show k ++ ":" ++ show v) $ Salak.Trie.toList t

instance Foldable Trie where
  {-# INLINE foldr #-}
  foldr f b Trie{..} = foldr (flip (foldr f)) (go tvar) tmap
    where
      {-# INLINE go #-}
      go (Just x) = f x b
      go _        = b

instance Traversable Trie where
  {-# INLINE traverse #-}
  traverse f Trie{..} = Trie <$> go tvar <*> traverse (traverse f) tmap
    where
      {-# INLINE go #-}
      go (Just x) = Just <$> f x
      go _        = pure Nothing

-- | /O(1)/. A trie with a single element.
{-# INLINE singleton #-}
singleton :: v -> Trie v
singleton v = Trie (Just v) HM.empty

-- | /O(1)/. The empty trie.
{-# INLINE empty #-}
empty :: Trie v
empty = Trie Nothing HM.empty

-- | /O(1)/. Return True if this trie is empty, False otherwise.
{-# INLINE null #-}
null :: Trie v -> Bool
null (Trie Nothing e) = HM.null e
null _                = False

-- | /O(log (n+m))/. Return True if the specified key is present in the trie, False otherwise.
{-# INLINE member #-}
member :: Eq v => Keys -> Trie v -> Bool
member k t = isJust (lookup k t)

{-# INLINE subTrie #-}
subTrie :: Key -> Trie v -> Trie v
subTrie key = fromMaybe empty . HM.lookup key . tmap

{-# INLINE subTries #-}
subTries :: Keys -> Trie v -> Trie v
subTries = flip (foldl' $ flip subTrie) . toKeyList

-- | /O(log (n+m))/. Return the primitive value to which the specified key is mapped,
-- or Nothing if this trie contains no mapping for the key.
{-# INLINE lookup #-}
lookup :: Eq v => Keys -> Trie v -> Maybe v
lookup keys = tvar . subTries keys

-- | /O(log n)/. Associate the specified value with the specified key in this trie.
-- If this trie previously contained a mapping for the key, the old value is replaced.
{-# INLINE insert #-}
insert :: Eq v => Keys -> v -> Trie v -> Trie v
insert ks v = alter (const $ Just v) ks

-- | /O(log m)/. The expression (`modify` k f trie) modifies the sub trie at k.
{-# INLINE modify #-}
modify :: Eq v => Key -> (Trie v -> Trie v) -> Trie v -> Trie v
modify k f (Trie v m) = Trie v $ HM.alter (convert . f . fromMaybe empty) k m

{-# INLINE convert #-}
convert :: Eq v => Trie v -> Maybe (Trie v)
convert x = if x == empty then Nothing else Just x

-- | /O(log (n+m))/. The expression (`modify'` ks f trie) modifies the sub trie at ks.
{-# INLINE modify' #-}
modify' :: Eq v => (Trie v -> Trie v) -> Keys -> Trie v -> Trie v
modify' f = foldr modify f . toKeyList

-- | /O(1)/. The expression (update f trie) updates the primitive value in the trie.
{-# INLINE update #-}
update :: Eq v => (Maybe v -> Maybe v) -> Trie v -> Trie v
update = flip alter mempty

-- | /O(n)/. The expression (update f ks trie) updates the primitive value of sub trie at ks.
{-# INLINE alter #-}
alter :: Eq v => (Maybe v -> Maybe v) -> Keys -> Trie v -> Trie v
alter f = modify' (\(Trie a b) -> Trie (f a) b)

-- | /O(n*m)/. Return a list of this tries's elements. The list is produced lazily.
toList :: Trie v -> [(Keys, v)]
toList = go D.empty
  where
    {-# INLINE go #-}
    go p (Trie (Just v) m) = (Keys p, v) : g2 p m
    go p (Trie _        m) = g2 p m
    {-# INLINE g2 #-}
    g2 p m = concat $ g3 p <$> HM.toList m
    {-# INLINE g3 #-}
    g3 p (k,t) = go (D.snoc p k) t

-- | /O(n*m*log n)/. Construct a trie with the supplied mappings.
-- If the list contains duplicate mappings, the later mappings take precedence.
{-# INLINE fromList #-}
fromList :: Eq v => [(Keys, v)] -> Trie v
fromList = foldr (uncurry insert) empty

-- | /O(n)/. Filter this trie by retaining only elements which values satisfy a predicate.
{-# INLINE filter #-}
filter :: Eq v => (v -> Bool) -> Trie v -> Trie v
filter f (Trie v m) = if ok v then Trie v go else Trie Nothing go
  where
    {-# INLINE ok #-}
    ok (Just x) = f x
    ok _        = False
    {-# INLINE go #-}
    go = HM.mapMaybe (convert . filter f) m

-- | /O(n+m)/. The union of two tries.
-- If a key occurs in both tries, the provided function (first argument) will be used to compute the result.
{-# INLINE unionWith #-}
unionWith :: Eq v => (Maybe v -> Maybe v -> Maybe v) -> Trie v -> Trie v -> Trie v
unionWith f (Trie v1 m1) (Trie v2 m2) = Trie (f v1 v2) $ HM.unionWith (unionWith f) m1 m2

-- | /O(n+m)/. The union of two tries.
-- All the keys will be calculated by the provided function.
{-# INLINE unionWith' #-}
unionWith' :: (Maybe v -> Maybe v -> Maybe v3) -> Trie v -> Trie v -> Trie v3
unionWith' f (Trie v1 m1) (Trie v2 m2) = Trie (f v1 v2) $ foldr go HM.empty $ HM.keys $ HM.union m1 m2
  where
    {-# INLINE go #-}
    go k =
      let x1 = fromMaybe empty $ HM.lookup k m1
          x2 = fromMaybe empty $ HM.lookup k m2
      in HM.insert k (unionWith' f x1 x2)

