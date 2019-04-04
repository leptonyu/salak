{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Salak.Toml(
    TOML(..)
  , loadToml
  ) where

import           Control.Monad          (foldM, (>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Writer
import qualified Data.HashMap.Strict    as HM
import qualified Data.List.NonEmpty     as N
import qualified Data.Text.IO           as IO
import           Data.Time
import           Salak
import           Salak.Load
import           Toml                   hiding (TOML)
import qualified Toml                   as T

data TOML = TOML

instance HasLoad TOML where
  loaders _ = (, loadToml) <$> ["toml", "tml"]

toSs :: Key -> [Selector]
toSs (Key ps) = toS <$> N.toList ps

toS :: Piece -> Selector
toS = SStr . unPiece

loadTOML :: Monad m => T.TOML -> Priority -> Source -> WriterT [String] m Source
loadTOML T.TOML{..} i = foldPairs       i tomlPairs
                    >=> foldTables      i tomlTables
                    >=> foldTableArrays i tomlTableArrays
foldToml go m s = HM.foldlWithKey' (\ms k v -> ms >>= go k v) (return s) m
foldPairs  i      = foldToml (\k -> updateSources (toSs k) . insertAnyValue i)
foldTables i      = foldToml (const go)
  where
    go (Leaf   k     toml) = updateSources (toSs k) (loadTOML toml i)
    go (Branch k v' tomap) = updateSources (toSs k) (maybe return (`loadTOML` i) v' >=> foldTables i tomap)
foldTableArrays i = foldToml (\k v -> updateSources (toSs k) (foldArray (N.toList v) (`loadTOML` i)))

insertAnyValue :: Monad m => Priority -> AnyValue -> Source -> m Source
insertAnyValue i (AnyValue (Array   b))             = foldArray b (insertAnyValue i . AnyValue)
insertAnyValue i (AnyValue (Bool    b))             = return . insertSource (VBool  i b)
insertAnyValue i (AnyValue (Integer b))             = return . insertSource (VNum   i $ fromIntegral b)
insertAnyValue i (AnyValue (Double  b))             = return . insertSource (VNum   i $ realToFrac b)
insertAnyValue i (AnyValue (Text    b))             = return . insertSource (VStr   i b)
insertAnyValue i (AnyValue (Local   b))             = return . insertSource (VLTime i b)
insertAnyValue i (AnyValue (Day     b))             = return . insertSource (VDay   i b)
insertAnyValue i (AnyValue (Hours   b))             = return . insertSource (VHour  i b)
insertAnyValue i (AnyValue (Zoned (ZonedTime a b))) = return . insertSource (VZTime i b a)

foldArray :: Monad m => [a] -> (a -> Source -> m Source) -> Source -> m Source
foldArray a g s = foldM (\s' (ix,x) -> updateSource (SNum ix) (g x) s') s $ zip [0..] a

loadToml :: MonadIO m => FilePath -> LoadSalakT m ()
loadToml file = do
  re <- liftIO $ parse <$> IO.readFile file
  case re of
      Left  e -> addErr (show e)
      Right a -> loadFile (defReload file $ loadToml file) (loadTOML a)
