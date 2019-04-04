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
loadTOML T.TOML{..} i s = HM.foldlWithKey' (g1 i) (return s) tomlPairs
                        >>= h2 i tomlTables

g1 i ms k v' = ms >>= updateSources (toSs k) (f1 i v')
g2 i ms _ v' = ms >>= f2 i v'

f1 :: Monad m => Priority -> AnyValue -> Source -> m Source
f1 i (AnyValue (Array   b)) = \s -> foldM (\s' (ix,x) -> updateSource (SNum ix) (f1 i $ AnyValue x) s') s $ zip [0..] b
f1 i (AnyValue (Bool    b)) = return . insertSource (VBool  i b)
f1 i (AnyValue (Integer b)) = return . insertSource (VNum   i $ fromIntegral b)
f1 i (AnyValue (Double  b)) = return . insertSource (VNum   i $ realToFrac b)
f1 i (AnyValue (Text    b)) = return . insertSource (VStr   i b)
f1 i (AnyValue (Local   b)) = return . insertSource (VLTime i b)
f1 i (AnyValue (Day     b)) = return . insertSource (VDay   i b)
f1 i (AnyValue (Hours   b)) = return . insertSource (VHour  i b)
f1 i (AnyValue (Zoned (ZonedTime a b))) = return . insertSource (VZTime i b a)
-- f1 i _ = return
f2 :: Monad m => Priority -> PrefixTree T.TOML -> Source -> WriterT [String] m Source
f2 i (Leaf   k     toml) = updateSources (toSs k) (loadTOML toml i)
f2 i (Branch k v' tomap) = updateSources (toSs k) (h1 i v' >=> h2 i tomap)
h1 :: Monad m => Priority -> Maybe T.TOML -> Source -> WriterT [String] m Source
h1 i (Just t) = loadTOML t i
h1 _ _        = return
h2 :: Monad m => Priority -> PrefixMap T.TOML -> Source -> WriterT [String] m Source
h2 i m s = HM.foldlWithKey' (g2 i) (return s) m

loadToml :: MonadIO m => FilePath -> LoadSalakT m ()
loadToml file = do
  re <- liftIO $ parse <$> IO.readFile file
  case re of
      Left  e -> addErr (show e)
      Right a -> loadFile (defReload file $ loadToml file) (loadTOML a)
