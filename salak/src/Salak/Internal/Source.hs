module Salak.Internal.Source where

import           Control.Concurrent.MVar
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Set                as S
import           Data.Text               (Text)
import           GHC.Stack               (CallStack)
import           Salak.Internal.Key
import           Salak.Internal.Val
import qualified Salak.Trie              as T

type Source = T.Trie Vals
type TraceVals = ([String], Vals)
type TraceSource = T.Trie TraceVals

-- | Reload result, show erros or changes.
data ReloadResult = ReloadResult
  { hasError :: !Bool     -- ^ If reload process has errors.
  , msgs     :: ![String] -- ^ If hasError then this show error messages, else this show change logs.
  } deriving Show

type QFunc = Source -> IO (IO ())

type LFunc = CallStack -> Text -> IO ()

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
    {-# INLINE go #-}
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
    {-# INLINE t1 #-}
    t1   = fmap snd t
    {-# INLINE list #-}
    list = fmap (\(k,v)->(show k,v)) $ T.toList $ fmap fst t

genSource :: (Foldable f, ToKeys k, ToValue v) => Int -> f (k,v) -> TraceSource
genSource i = foldr go T.empty
  where
    {-# INLINE go #-}
    go (k,v) x = case toKeys k of
      Left  e  -> T.alter (setErr0 e) mempty x
      Right k' -> T.alter (setVal0 $ Val i $ toVal v) k' x

{-# INLINE fmt #-}
fmt :: ModType -> Int -> String -> String -> String
fmt m i s n = concat ['#' : show i, ' ' : show m, ' ' : s ,  ' ' : n]

{-# INLINE fmtMod #-}
fmtMod :: Int -> String -> HashMap String ModType -> [String]
fmtMod i name cs = fmap (\(k,v)-> fmt v i k name) (HM.toList cs)

loadSource :: (Int -> IO TraceSource) -> Int -> TraceSource -> IO TraceSource
loadSource f i ts = T.unionWith go ts <$> f i
  where
    {-# INLINE go #-}
    go Nothing Nothing               = Nothing
    go (Just v) Nothing              = Just v
    go Nothing (Just v)              = Just v
    go (Just (e1,v1)) (Just (e2,v2)) = case modVals' v2 v1 of
      Left  e -> Just (e:e1++e2, v2)
      Right v -> Just (e1++e2,v)

{-# INLINE traceError #-}
traceError :: Maybe TraceVals -> [String]
traceError Nothing      = []
traceError (Just (e,_)) = e

{-# INLINE traceVals #-}
traceVals :: Maybe TraceVals -> Vals
traceVals Nothing      = emptyVals
traceVals (Just (_,v)) = v

{-# INLINE setErr0 #-}
setErr0 :: String -> Maybe TraceVals -> Maybe TraceVals
setErr0 e (Just (a,c)) = Just (e:a,c)
setErr0 e _            = Just ([e], emptyVals)

{-# INLINE setVal0 #-}
setVal0 :: Val Value -> Maybe TraceVals -> Maybe TraceVals
setVal0 v tv =
  let tv2 = traceVals tv
  in case modVals v tv2 of
      Left  e -> Just (e : traceError tv, tv2)
      Right x -> Just (traceError tv, x)

{-# INLINE setVal #-}
setVal :: ToValue v => Int -> v -> TraceSource -> TraceSource
setVal i v = T.update (setVal0 $ Val i $ toVal v)
