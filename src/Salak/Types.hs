{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Salak.Types where

import           Control.Applicative  ((<|>))
import           Control.Monad.State
import           Data.Attoparsec.Text
import qualified Data.IntMap.Strict   as MI
import           Data.List            (intercalate)
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.PQueue.Min      as Q
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)
import qualified Data.Text            as T

type Priority = Int

data Value
  = VStr  Priority !Text
  | VNum  Priority !Scientific
  | VBool Priority !Bool
  deriving (Eq, Show, Ord)

type QV = Q.MinQueue Value

getPriority :: Value -> Priority
getPriority (VStr  p _) = p
getPriority (VNum  p _) = p
getPriority (VBool p _) = p

data Reload = Reload
  { sourceName :: Text
  , reload     :: Priority -> IO Source
  }

instance Show Reload where
  show (Reload s _) = T.unpack s

emptyReload :: Text -> Reload
emptyReload s = Reload s (\_ -> return emptySource)

type PriorityEnv = MI.IntMap Reload

nullSource :: Source -> Bool
nullSource (Source _ q is ts) = Q.null q && MI.null is && M.null ts

getQ :: Source -> QV
getQ (Source _ q _ _) = q

replaceQ :: String -> Priority -> QV -> QV -> ([String], QV)
replaceQ s i nq q =
  let (a,b) = Q.partition ((==i) . getPriority) q
  in if a == nq then ([], q) else case Q.getMin nq of
    Just v -> ([ (if Q.null a then "Add " else "Mod ") ++ s], Q.insert v b)
    _      -> (if Q.null a then [] else ["Del " ++ s], b)

data Source' v = Source [String] v (MI.IntMap (Source' v)) (M.Map Text (Source' v)) deriving (Eq, Functor)

type Source = Source' QV

instance Show Source where
  show = unlines . go ""
    where
      go p (Source _ q is ts) = (if Q.null q then [] else [ p ++ "=" ++ show q ])
        ++ concat ((\(k,v) -> go (p ++ "[" ++ show k ++ "]") v) <$> MI.toList is)
        ++ concat ((\(k,v) -> go (if null p then T.unpack k else p ++ "." ++ T.unpack k) v) <$> M.toList  ts)

emptySource :: Source
emptySource = Source [] Q.empty MI.empty M.empty

instance Foldable Source' where
  foldr f b s@(Source _ _ is ts) = foldr go (foldr go (go s b) is) ts
    where
      go (Source _ q _ _) = f q

foldSource :: (Value -> b -> b) -> b -> Source -> b
foldSource f = foldr (\q b -> maybe b (`f` b) $ Q.getMin q)

sizeSouce :: Source -> Int
sizeSouce = foldSource (const (+1)) 0

extractErr :: Source -> ([String], Source)
extractErr (Source es q is ts) =
  let (ise, is') = MI.mapAccum go es  is
      (tse, ts') =  M.mapAccum go ise ts
  in (tse, Source [] q is' ts')
  where
    go e s = let (e', s') = extractErr s in (e ++ e', s')

replace = replace' []

replace' :: [Selector] -> Priority -> Source -> Source -> Source
replace' ss i (Source _ nq nis nts) (Source es q is ts) =
  let (ms, q')  = replaceQ (toKey ss) i nq q
      (isa,isb) = MI.partition nullSource $ MI.mapWithKey (g2.SNum) $ MI.unionWithKey (go.SNum) (f 0 nis) (f 1 is)
      (tsa,tsb) =  M.partition nullSource $  M.mapWithKey (g2.STxt) $  M.unionWithKey (go.STxt) (f 0 nts) (f 1 ts)
  in Source (g $ Source (es ++ ms) Q.empty isa tsa) q' isb tsb
  where
    f x = ((x::Int,) <$>)
    g   = fst . extractErr
    go st (_, s) (_, s') = (2, replace' (st:ss) i s s')
    g2 st (0, s) = replace' (st:ss) i s emptySource
    g2 st (1, s) = replace' (st:ss) i emptySource s
    g2 _  (_, s) = s

data Selector
  = STxt !Text
  | SNum !Int
  deriving Eq

instance Show Selector where
  show (STxt x) = T.unpack x
  show (SNum i) = "[" ++ show i ++ "]"

toKey :: [Selector] -> String
toKey = intercalate "." . go . reverse
  where
    go (a@(STxt _):b@(SNum _):cs) = (show a ++ show b) : go cs
    go (a:bs)                     = show a : go bs
    go []                         = []

selectors :: Text -> Either String [Selector]
selectors = go . parse exprs . flip T.snoc '\n'
  where
    go (Done i r) = if i /= "\n" then Left $ "uncomplete parse" ++ T.unpack i else Right r
    go a          = Left (show a)

exprs :: Parser [Selector]
exprs = concat <$> ( (expr <|> return []) `sepBy` char '.')

-- xx
-- xx.xx
-- xx.xx[0]
-- xx.xx[1].xx
expr :: Parser [Selector]
expr = do
  name <- T.pack <$> do
    a <- choice [letter, digit]
    b <- many' (choice [letter, digit, char '-',  char '_'])
    return (a:b)
  (paren decimal >>= \i -> return [STxt name, SNum i]) <|> return [STxt name]
  where
    paren e = do
      _  <- char '['
      ex <- e
      _  <- char ']'
      return ex

addErr :: String -> Source -> Source
addErr e (Source es a b c) = Source (e:es) a b c

insert :: Text -> Value -> Source -> Source
insert k v s = case selectors k of
  Left  e  -> addErr e s
  Right k' -> insert' k' v s

insert' :: [Selector] -> Value -> Source -> Source
insert' [] v (Source es q is ts) = Source es (Q.insert v q) is ts
insert' (STxt n:ss) v (Source es q is ts) = Source es q is $ M.alter (Just . insert' ss v . fromMaybe emptySource) n ts
insert' (SNum i:ss) v (Source es q is ts) = Source es q (MI.alter (Just . insert' ss v . fromMaybe emptySource) i is) ts

data SourcePack = SourcePack [Selector] Int Source PriorityEnv deriving Show

emptySourcePack = SourcePack [] 0 emptySource MI.empty

mapSource :: (Source -> Source) -> SourcePack -> SourcePack
mapSource f (SourcePack ss i s it) = SourcePack ss i (f s) it

select :: SourcePack -> Selector -> SourcePack
select (SourcePack ss i (Source _ _ _ ts) it) s@(STxt n) = SourcePack (s:ss) i (fromMaybe emptySource $  M.lookup n ts) it
select (SourcePack ss i (Source _ _ is _) it) s@(SNum n) = SourcePack (s:ss) i (fromMaybe emptySource $ MI.lookup n is) it

addErr' :: String -> SourcePack -> SourcePack
addErr' e = mapSource (addErr e)

extractErr' :: SourcePack -> ([String], SourcePack)
extractErr' (SourcePack ss i s it) = let (es, s') = extractErr s in (es, SourcePack ss i s' it)

loadFile :: Reload -> SourcePack -> (Priority -> Source -> Source) -> SourcePack
loadFile name (SourcePack ss i s env) go = SourcePack ss (i+1) (go i s) $ MI.insert i name env

load
  :: (Functor f, Foldable f)
  => Reload
  -> f a
  -> (Priority -> a -> (Text, Value))
  -> SourcePack
  -> SourcePack
load name fa f sp = loadFile name sp $ \i s -> foldl (go i) s fa
  where
    go i s a = let (k,v) = f i a in insert k v s

loadMock :: Monad m => [(Text, Text)] -> SourcePackT m ()
loadMock fs = modify $ load (emptyReload "") fs (\i (k,v) -> (k, VStr i v))

type SourcePackT = StateT SourcePack

runSourcePackT :: Monad m => SourcePackT m a -> m ([String], SourcePack)
runSourcePackT ac = extractErr' <$> execStateT ac emptySourcePack

