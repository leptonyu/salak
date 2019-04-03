{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
module Salak.Types.Source where

import           Control.Monad.Writer
import qualified Data.Map.Strict      as M
import           Data.Maybe
import qualified Data.Text            as T
import           Salak.Types.Selector
import           Salak.Types.Value

data SourceT v = Source
  { value    :: v
  , mapValue :: M.Map Selector (SourceT v)
  } deriving (Eq, Functor)

instance Foldable SourceT where
  foldr g b Source{..} = M.foldl (foldr g) (g value b) mapValue

type Source = SourceT QV

showKey :: String -> Selector -> String
showKey p (SStr k)
  | null p    = T.unpack k
  | otherwise = p <> "." <> T.unpack k
showKey p (SNum k) = p <> "[" <> show k <> "]"

instance Show Source where
  show = unlines . go ""
    where
      go p Source{..} = concat $ M.foldrWithKey (\k v b -> go (showKey p k) v : b) [ g2 p value] mapValue
      g2 x v = if nullQ v then [] else [x <> "=" <> show v]

emptySource :: Source
emptySource = Source mempty mempty

foldSource :: (Value -> b -> b) -> b -> Source -> b
foldSource f = foldr (\q b -> maybe b (`f` b) $ getQ q)

sizeSource :: Source -> Int
sizeSource = foldSource  (const (+1)) 0

nullSource :: Source -> Bool
nullSource = foldSource (\_ _ -> False) True

selectSource :: Selector -> Source -> Source
selectSource n Source{..} = fromMaybe emptySource $ M.lookup n mapValue

updateSource :: Monad m => Selector -> (Source -> m Source) -> Source -> m Source
updateSource n f ss = do
  ss' <- f $ selectSource n ss
  return $ ss { mapValue = M.insert n ss' (mapValue ss) }

updateSources :: Monad m => [Selector] -> (Source -> m Source) -> Source -> m Source
updateSources = flip (foldr updateSource)

replace = replace' []

replace' :: [Selector] -> Priority -> Source -> Source -> Writer [String] Source
replace' ss i ns os = do
  q' <- replaceQ (toKey ss) i (value ns) (value os)
  m' <- mapM snd $ M.mapWithKey g2 $ M.unionWithKey go (f 0 $ mapValue ns) (f 1 $ mapValue os)
  return (Source q' $ M.filter (not.nullSource) m')
  where
    f j m = (j :: Int,) . return <$> m
    go k (_, a) (_, b) = (2, a >>= \a' -> b >>= \b' -> replace' (k:ss) i a' b')
    g2 k (0, a) = (0, a >>= \a' -> replace' (k:ss) i a' emptySource)
    g2 k (1, a) = (1, a >>= \a' -> replace' (k:ss) i emptySource a')
    g2 _ (_, a) = (2 :: Int, a)

insert :: Monad m => T.Text -> Value -> Source -> WriterT [String] m Source
insert k v s = case selectors k of
  Left  e  -> tell [e] >> return s
  Right k' -> return (insert' k' v s)

insert' :: [Selector] -> Value -> Source -> Source
insert' ns v = foldr go (insertSource v) ns
  where
    go n f s = s { mapValue = M.alter (Just . f . fromMaybe emptySource) n $ mapValue s}

insertSource :: Value -> Source -> Source
insertSource v s = s { value = insertQ v $ value s}
