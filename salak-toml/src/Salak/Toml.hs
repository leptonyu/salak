{-# LANGUAGE GADTs             #-}
-- |
-- Module:      Salak.Toml
-- Copyright:   (c) 2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Toml support for "Salak".
--
module Salak.Toml(
    TOML(..)
  , loadToml
  , runSalakWithToml
  ) where

import           Control.Exception   (Exception, throwIO)
import qualified Data.HashMap.Strict as HM
import           Data.List           (foldl')
import qualified Data.List.NonEmpty  as N
import           Data.Text           (Text)
import qualified Data.Text.IO        as IO
import           Data.Time
import           Salak
import           Salak.Internal
import qualified Salak.Trie          as TR
import           Toml                hiding (Key, TOML, Value)
import qualified Toml                as T


runSalakWithToml :: (MonadCatch m, MonadIO m) => FilePath -> RunSalakT m a -> m a
runSalakWithToml name = runSalakWith name TOML

-- | TOML notation for `loadToml`
data TOML = TOML

instance HasLoad TOML where
  loaders _ = (, loadToml) <$> ["toml", "tml"]

{-# INLINE toSs #-}
toSs :: T.Key -> Keys
toSs (T.Key ps) = fromKeys $ toS <$> N.toList ps

{-# INLINE toS #-}
toS :: Piece -> Key
toS = KT . unPiece

loadTOML :: Int -> T.TOML -> TraceSource -> TraceSource
loadTOML i T.TOML{..}
  = foldPairs       tomlPairs
  . foldTables      tomlTables
  . foldTableArrays tomlTableArrays
  where
    {-# INLINE foldToml #-}
    foldToml = flip . HM.foldlWithKey'
    {-# INLINE foldPairs #-}
    foldPairs       = foldToml (\s k v -> TR.modify' (insertAnyValue v) (toSs k) s)
    {-# INLINE foldTableArrays #-}
    foldTableArrays = foldToml (\s _ v -> foldArray (loadTOML i) (N.toList v) s)
    {-# INLINE foldTables #-}
    foldTables      = foldToml (\s _ v -> go v s)
    {-# INLINE go #-}
    go (Leaf   k   toml)  = TR.modify' (loadTOML i toml)  (toSs k)
    go (Branch k v tomap) = TR.modify' (foldTables tomap) (toSs k) . maybe id (loadTOML i) v
    {-# INLINE insertAnyValue #-}
    insertAnyValue :: AnyValue -> TraceSource -> TraceSource
    insertAnyValue (AnyValue (Array   b))             = foldArray (insertAnyValue . AnyValue) b
    insertAnyValue (AnyValue (Bool    b))             = setVal i (VB  b)
    insertAnyValue (AnyValue (Integer b))             = setVal i (VI $ fromIntegral b)
    insertAnyValue (AnyValue (Double  b))             = setVal i (VI $ realToFrac   b)
    insertAnyValue (AnyValue (Text    b))             = setVal i (VT  b)
    insertAnyValue (AnyValue (Local   b))             = setVal i (VLT b)
    insertAnyValue (AnyValue (Day     b))             = setVal i (VD  b)
    insertAnyValue (AnyValue (Hours   b))             = setVal i (VH  b)
    insertAnyValue (AnyValue (Zoned   b))             = setVal i (VU  $ zonedTimeToUTC b)
    {-# INLINE foldArray #-}
    foldArray :: (a -> TraceSource -> TraceSource) -> [a] -> TraceSource -> TraceSource
    foldArray f = flip (foldl' (\t (j,a) -> TR.modify (KI j) (f a) t)) . zip [0..]

newtype TomlException = TomlException Text deriving Show

instance Exception TomlException

-- | Load Toml
loadToml :: FilePath -> LoadSalak ()
loadToml file = loadTrie True file $ \i -> do
  re <- T.parse <$> IO.readFile file
  case re of
      Left  (T.ParseException e) -> throwIO (TomlException e)
      Right a                    -> return  (loadTOML i a TR.empty)
