{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module Salak.Load.Toml where


import           Control.Monad          (foldM, (>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State
import qualified Data.HashMap.Strict    as HM
import qualified Data.List.NonEmpty     as N
import qualified Data.Text.IO           as IO
import           Data.Time
import           Salak.Types
import           Salak.Types.Selector
import           Salak.Types.Source
import           Salak.Types.Value
import           Toml

toSs :: Key -> [Selector]
toSs (Key ps) = toS <$> N.toList ps

toS :: Piece -> Selector
toS = SStr . unPiece

loadTOML :: Reload -> TOML -> SourcePack -> SourcePack
loadTOML name v sp = loadFile name sp $ go v
  where
    go TOML{..} i s = do
      s1 <- HM.foldlWithKey' (g1 i) (return s ) tomlPairs
      s2 <- HM.foldlWithKey' (g2 i) (return s1) tomlTables
      return s2
    g1 i ms k v' = ms >>= updateSources (toSs k) (f1 i v')
    -- TOML replace
    g2 i ms _ v' = ms >>= f2 i v'
    f1 :: Monad m => Priority -> AnyValue -> Source -> m Source
    f1 i (AnyValue (Bool    b)) = return . insertSource (VBool i b)
    f1 i (AnyValue (Integer b)) = return . insertSource (VNum  i $ fromIntegral b)
    f1 i (AnyValue (Double  b)) = return . insertSource (VNum  i $ realToFrac b)
    f1 i (AnyValue (Text    b)) = return . insertSource (VStr  i b)
    f1 i (AnyValue (Date    b)) = return . insertSource (VDate i $ f3 b)
    f1 i (AnyValue (Array   b)) = \s -> foldM (\s' (ix,x) -> updateSource (SNum ix) (f1 i $ AnyValue x) s') s $ zip [0..] b
    f2 :: Monad m => Priority -> PrefixTree TOML -> Source -> m Source
    f2 i (Leaf   k     toml) = updateSources (toSs k) (go toml i)
    f2 i (Branch k v' tomap) = updateSources (toSs k) (h1 i v' >=> h2 i tomap)
    h1 :: Monad m => Priority -> Maybe TOML -> Source -> m Source
    h1 i (Just t) = go t i
    h1 _ _        = return
    h2 :: Monad m => Priority -> PrefixMap TOML -> Source -> m Source
    h2 i m s = HM.foldlWithKey' (\ms k t -> ms >>= updateSource (toS k) (f2 i t)) (return s) m
    f3 (Zoned (ZonedTime a b)) = DV1 b a
    f3 (Local b)               = DV2 b
    f3 (Day   b)               = DV3 b
    f3 (Hours b)               = DV4 b

loadToml :: MonadIO m => FilePath -> SourcePackT m ()
loadToml file = do
    re <- liftIO $ parse <$> IO.readFile file
    modify $ \sp ->case re of
        Left  e -> addErr' (show e) sp
        Right a -> loadTOML (defReload file $ loadToml file) a sp

tryLoadToml :: MonadIO m => FilePath -> SourcePackT m ()
tryLoadToml = tryLoadFile loadToml
