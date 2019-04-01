{-# LANGUAGE TupleSections #-}
module Salak.Load.Yaml where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State
import qualified Data.Aeson             as A
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import qualified Data.Yaml              as Y
import           Salak
import           Salak.Load

loadJSON :: Reload -> A.Value -> SourcePack -> SourcePack
loadJSON name v sp = loadFile name sp $ go v
  where
    go (A.Object m) i s = foldl (g2 i) (return s) $ g3 <$> HM.toList m
    go (A.Array  m) i s = foldl (g2 i) (return s) $ zip (g4 <$> [0..]) $ V.toList m
    go (A.String m) i s = return $ insertSource (VStr  i m) s
    go (A.Number m) i s = return $ insertSource (VNum  i m) s
    go (A.Bool   m) i s = return $ insertSource (VBool i m) s
    go A.Null       _ s = return s
    g2 i ms (k, v') = ms >>= updateSources k (go v' i)
    g3 (k,x) = (simpleSelectors k, x)
    g4 i     = [SNum i]

loadYaml :: MonadIO m => FilePath -> SourcePackT m ()
loadYaml file = do
  v <- liftIO $ Y.decodeFileEither file
  modify $ \sp -> case v of
    Left  e -> addErr' (show e) sp
    Right a -> loadJSON (defReload file $ loadYaml file) a sp

data YAML = YAML

instance HasLoad YAML where
  loaders _ = (, loadYaml) <$> ["yaml", "yml"]
