{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoOverloadedLists #-}
module Salak.Internal.Source where

import           Control.Concurrent.MVar
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Set                as S
import           Salak.Internal.Key
import           Salak.Internal.Val
import qualified Salak.Trie              as T

type Source = T.Trie Vals
type TraceSource = T.Trie ([String], Vals)

-- | Reload result, show erros or changes.
data ReloadResult = ReloadResult
  { hasError :: !Bool     -- ^ If reload process has errors.
  , msgs     :: ![String] -- ^ If hasError then this show error messages, else this show change logs.
  } deriving Show

type QFunc = Source -> Either String (IO ())

type LFunc = String -> IO ()

data SourcePack = SourcePack
  { source :: !Source
  , origin :: !Source
  , kref   :: !(S.Set Keys)
  , pref   :: !Keys
  , qref   :: !(MVar QFunc)
  , lref   :: !(MVar LFunc)
  , reload :: !(IO ReloadResult)
  }

diff :: Source -> Source -> T.Trie ModType
diff = T.unionWith' go
  where
    go Nothing Nothing = Nothing
    go (Just a) Nothing  = if nullVals a then Nothing else Just Add
    go Nothing (Just a)  = if nullVals a then Nothing else Just Del
    go (Just a) (Just b)
      | nullVals a && nullVals b = Nothing
      | nullVals a               = Just Del
      | nullVals b               = Just Add
      | otherwise                =
        let Val i x = minimumVals a
            Val j y = minimumVals b
        in if i==j && x==y then Nothing else Just Mod

extract :: Source -> TraceSource -> (Source, T.Trie ModType, [String])
extract o t =
  ( t1
  , diff t1 o
  , concatMap (\(k,v)-> fmap (k++) v) list)
  where
    t1   = fmap snd t
    list = fmap (\(k,v)->(show k,v)) $ T.toList $ fmap fst t

gen :: (Foldable f, ToKeys k, ToValue v) => Int -> f (k,v) -> TraceSource
gen i = foldr go T.empty
  where
    go (k,v) x = case toKeys k of
      Left  e  -> T.alter (g3 e) mempty x
      Right k' -> T.alter (g2 $ Val i $ toVal v) k' x
    g2 v (Just (a,c)) = case modVals v c of
      Left  e -> Just (e:a, c)
      Right d -> Just (a,   d)
    g2 v _            = case singletonVals v of
      Left  e -> Just ([e], emptyVals)
      Right d -> Just ([], d)
    g3 e (Just (a,c)) = Just (e:a,c)
    g3 e _            = Just ([e], emptyVals)

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
    go (Just (e1,v1)) (Just (e2,v2)) = case modVals' v2 v1 of
      Left  e -> Just (e:e1++e2, v2)
      Right v -> Just (e1++e2,v)

setVal :: ToValue v => Int -> v -> TraceSource -> TraceSource
setVal i v = T.update go
  where
    go Nothing       = case modVals (Val i $ toVal v) emptyVals of
      Left  e -> Just ([e], emptyVals)
      Right x -> Just ([], x)
    go (Just (e,vs)) = case modVals (Val i $ toVal v) vs of
      Left  e1 -> Just (e1:e, vs)
      Right x  -> Just (e, x)
