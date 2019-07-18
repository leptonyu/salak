{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
-- |
-- Module:      Salak.Toml
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Toml support for "Salak".
--
module Salak.Toml(
    TOML(..)
  , loadToml
  ) where

import           Control.Exception   (Exception, throwIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as N
import           Data.Text           (Text)
import qualified Data.Text.IO        as IO
import           Data.Time
import           Salak
import           Salak.Internal
import qualified Salak.Trie          as TR
import           Toml                hiding (Key, TOML, Value)
import qualified Toml                as T

-- | TOML notation for `loadToml`
data TOML = TOML

instance HasLoad TOML where
  loaders _ = (, loadToml) <$> ["toml", "tml"]

toSs :: T.Key -> [Key]
toSs (T.Key ps) = toS <$> N.toList ps

toS :: Piece -> Key
toS = KT . unPiece

loadTOML :: Int -> T.TOML -> TraceSource-> TraceSource
loadTOML i T.TOML{..}
  = foldPairs       tomlPairs
  . foldTables      tomlTables
  . foldTableArrays tomlTableArrays
  where
    foldToml go p t = HM.foldlWithKey' go t p
    foldPairs       = foldToml (\s k v -> TR.modify' (Keys $ toSs k) (insertAnyValue i v) s)
    foldTableArrays = foldToml (\s _ v -> foldArray (N.toList v) (loadTOML i) s)
    foldTables      = foldToml (\s _ v -> go v s)
      where
        go (Leaf   k toml)    = TR.modify' (Keys $ toSs k) (loadTOML i toml)
        go (Branch k v tomap) = TR.modify' (Keys $ toSs k) (foldTables tomap) . maybe id (loadTOML i) v
insertAnyValue :: Int -> AnyValue -> TraceSource -> TraceSource
insertAnyValue i (AnyValue (Array   b))             ts = foldArray b (insertAnyValue i . AnyValue) ts
insertAnyValue i (AnyValue (Bool    b))             ts = setVal i (VB  b) ts
insertAnyValue i (AnyValue (Integer b))             ts = setVal i (VI $ fromIntegral b) ts
insertAnyValue i (AnyValue (Double  b))             ts = setVal i (VI $ realToFrac   b) ts
insertAnyValue i (AnyValue (Text    b))             ts = setVal i (VT  b) ts
insertAnyValue i (AnyValue (Local   b))             ts = setVal i (VLT b) ts
insertAnyValue i (AnyValue (Day     b))             ts = setVal i (VD  b) ts
insertAnyValue i (AnyValue (Hours   b))             ts = setVal i (VH  b) ts
insertAnyValue i (AnyValue (Zoned (ZonedTime a b))) ts = setVal i (VZT b a) ts

foldArray :: [a] -> (a -> TraceSource -> TraceSource) -> TraceSource -> TraceSource
foldArray as f ts = foldl go ts $ zip [0..] as
  where
    go t (i, a) = TR.modify (KI i) (f a) t

newtype TomlException = TomlException Text deriving Show

instance Exception TomlException

-- | Load Toml
loadToml :: FilePath -> LoadSalak ()
loadToml file = loadTrie True file $ \i -> do
  re <- T.parse <$> IO.readFile file
  case re of
      Left  (T.ParseException e) -> throwIO (TomlException e)
      Right a                    -> return  (loadTOML i a TR.empty)
