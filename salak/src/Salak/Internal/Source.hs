{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoOverloadedLists #-}
module Salak.Internal.Source where

import           Control.Concurrent.MVar
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Heap               as H
import           Data.Maybe
import           Salak.Internal.Key
import           Salak.Internal.Val
import qualified Salak.Trie              as T

type Source = T.Trie Vals
type TraceSource = T.Trie ([String], Vals)

data ReloadResult = ReloadResult
  { hasError :: Bool
  , msgs     :: [String]
  }

type QFunc = Source -> Either String (IO ())

data SourcePack = SourcePack
  { source :: Source
  , pref   :: [Key]
  , qref   :: MVar QFunc
  , reload :: IO ReloadResult
  }

class Monad m => MonadSalak m where
  askSalak :: m SourcePack

askReload :: MonadSalak m => m (IO ReloadResult)
askReload = reload <$> askSalak

diff :: Source -> Source -> T.Trie ModType
diff = T.merge go
  where
    go Nothing Nothing = Nothing
    go (Just (Vals a)) Nothing  = if H.null a then Nothing else Just Add
    go Nothing (Just (Vals a))  = if H.null a then Nothing else Just Del
    go (Just (Vals a)) (Just (Vals b))
      | H.null a && H.null b = Nothing
      | H.null a             = Just Del
      | H.null b             = Just Add
      | otherwise            =
        let Val i x = H.minimum a
            Val j y = H.minimum b
        in if i==j && x==y then Nothing else Just Mod

extract :: Source -> TraceSource -> (Source, T.Trie ModType, [String])
extract o t =
  ( t1
  , diff t1 o
  , concatMap (\(k,v)-> fmap (k++) v) list)
  where
    t1   = fmap snd t
    list = fmap (\(k,v)->(show k,v)) $ T.toList $ fmap fst t

generate :: (Foldable f, ToKeys k, ToValue v) => Int -> f (k,v) -> TraceSource
generate i = foldr go T.empty
  where
    go (k,v) x = case toKeys k of
      Left  e  -> T.alter (g3 e) (Keys []) x
      Right k' -> T.alter (g2 $ Val i $ toVal v) k' x
    g2 v (Just (a,Vals c)) = Just (a, Vals $ H.insert v c)
    g2 v _                 = Just ([], Vals $ H.singleton v)
    g3 e (Just (a,c)) = Just (e:a,c)
    g3 e _            = Just ([e], Vals H.empty)

search :: (ToKeys k) => k -> Source -> Either String (Keys, Source)
search k t = fmap (go . unKeys) (toKeys k)
  where
    go ks = (Keys ks, foldl search1 t ks)

search2 :: Source -> [Key] -> Source
search2 = foldl search1

search1 :: Source -> Key -> Source
search1 (T.Trie _ m) key = fromMaybe T.empty $ HM.lookup key m

fmt :: ModType -> Int -> String -> String -> String
fmt m i s n = concat ['#' : show i, ' ' : show m, ' ' : s ,  ' ' : n]

fmtMod :: Int -> String -> HashMap String ModType -> [String]
fmtMod i name cs = fmap (\(k,v)-> fmt v i k name) (HM.toList cs)

loadSource :: (Int -> IO TraceSource) -> Int -> TraceSource -> IO TraceSource
loadSource f i ts = T.unionWith go ts <$> f i
  where
    go Nothing Nothing               = Nothing
    go (Just v) Nothing              = Just v
    go Nothing (Just v)              = Just v
    go (Just (e1,v1)) (Just (e2,v2)) = Just (e1++e2, modVals' v2 v1)

setVal :: ToValue v => Int -> v -> TraceSource -> TraceSource
setVal i v (T.Trie x m) = T.Trie (go x) m
  where
    go Nothing       = Just ([], modVals (Val i $ toVal v) emptyVals)
    go (Just (e,vs)) = Just (e,  modVals (Val i $ toVal v) vs)
