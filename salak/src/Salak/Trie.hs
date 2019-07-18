{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Salak.Trie where

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
import           Data.Tuple          (uncurry)
import           Salak.Internal.Key
import           Text.Show           (Show (..))

data Trie v = Trie !(Maybe v) !(HashMap Key (Trie v)) deriving (Eq, Functor)

instance Show v => Show (Trie v) where
  show t = intercalate "\n" $ map (\(k,v)->toKey (unKeys k) ++ ":" ++ show v) $ Salak.Trie.toList t

empty :: Trie v
empty = Trie Nothing HM.empty

null :: Eq v => Trie v -> Bool
null = (== empty)

instance Foldable Trie where
  foldr f b (Trie v m) = foldr (flip (foldr f)) (go v) m
    where
      go (Just x) = f x b
      go _        = b

member :: Eq v => Keys -> Trie v -> Bool
member k t = isJust (lookup k t)

lookup :: Eq v => Keys -> Trie v -> Maybe v
lookup (Keys keys) = go keys
  where
    go []     (Trie v _) = v
    go (k:ks) (Trie _ m) = case HM.lookup k m of
      Just t -> go ks t
      _      -> Nothing

insert :: Eq v => Keys -> v -> Trie v -> Trie v
insert ks v = alter (\_ -> Just v) ks

modify :: Eq v => Key -> (Trie v -> Trie v) -> Trie v -> Trie v
modify k f (Trie v m) = Trie v $ HM.alter (go . f . fromMaybe empty) k m
  where
    go x = if x == empty then Nothing else Just x


modify' :: Eq v => Keys -> (Trie v -> Trie v) -> Trie v -> Trie v
modify' (Keys ks) f = foldr modify f ks

modifyF :: (Monad m, Eq v) => Key -> (Trie v -> m (Trie v)) -> Trie v -> m (Trie v)
modifyF k f (Trie v m) = Trie v <$> HM.alterF go k m
  where
    go (Just w) = (\x -> if x == empty then Nothing else Just x) <$> f w
    go _        = return Nothing

alter :: Eq v => (Maybe v -> Maybe v) -> Keys -> Trie v -> Trie v
alter f (Keys keys) = go keys
  where
    go []     (Trie v m) = Trie (f v) m
    go (k:ks) (Trie v m) = Trie v $ HM.alter (g2 ks) k m
    g2 ks t = let t1 = go ks (fromMaybe empty t) in if t1 == empty then Nothing else Just t1

alterF :: (Functor f, Eq v) => (Maybe v -> f(Maybe v)) -> Keys -> Trie v -> f (Trie v)
alterF f (Keys keys) = go keys
  where
    go []     (Trie v m) = (`Trie` m) <$> f v
    go (k:ks) (Trie v m) = Trie v <$> HM.alterF (g2 ks) k m
    g2 ks t = g3 <$> go ks (fromMaybe empty t)
    g3 t = if t == empty then Nothing else Just t

toList :: Trie v -> [(Keys, v)]
toList = go []
  where
    go p (Trie (Just v) m) = (Keys $ reverse p, v) : g2 p m
    go p (Trie _        m) = g2 p m
    g2 p m = concat $ g3 p <$> HM.toList m
    g3 p (k,t) = go (k:p) t

fromList :: Eq v => [(Keys, v)] -> Trie v
fromList = foldr (uncurry insert) empty

filter :: Eq v => (v -> Bool) -> Trie v -> Trie v
filter f (Trie v m) = if ok v then Trie v go else Trie Nothing go
  where
    ok (Just x) = f x
    ok _        = False
    go = HM.mapMaybe (g2 . filter f) m
    g2 x = if x == empty then Nothing else Just x

partition :: Eq v => (v -> Bool) -> Trie v -> (Trie v, Trie v)
partition f t = (filter f t, filter (not.f) t)

unionWith :: Eq v => (Maybe v -> Maybe v -> Maybe v) -> Trie v -> Trie v -> Trie v
unionWith f (Trie v1 m1) (Trie v2 m2) = Trie (f v1 v2) $ HM.unionWith (unionWith f) m1 m2


merge :: (Maybe v -> Maybe v -> Maybe v3) -> Trie v -> Trie v -> Trie v3
merge f (Trie v1 m1) (Trie v2 m2) = Trie (f v1 v2) $ foldr go HM.empty $ HM.keys $ HM.union m1 m2
  where
    go k =
      let x1 = fromMaybe empty $ HM.lookup k m1
          x2 = fromMaybe empty $ HM.lookup k m2
      in HM.insert k (merge f x1 x2)

