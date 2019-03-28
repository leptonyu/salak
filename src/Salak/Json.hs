module Salak.Json where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State
import qualified Data.Aeson             as A
import qualified Data.HashMap.Strict    as HM
import qualified Data.IntMap.Strict     as MI
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Data.Text              (pack)
import qualified Data.Vector            as V
import qualified Data.Yaml              as Y
import           Salak.Types

loadJSON :: Reload -> A.Value -> SourcePack -> SourcePack
loadJSON name v sp = loadFile name sp $ go v
  where
    go (A.Object m) i s = foldl (g2 i) s $ HM.toList m
    go (A.Array  m) i s = foldl (g3 i) s $ zip [0..] $ V.toList m
    go (A.String m) i s = insert' [] (VStr  i m) s
    go (A.Number m) i s = insert' [] (VNum  i m) s
    go (A.Bool   m) i s = insert' [] (VBool i m) s
    go A.Null       _ s = s
    g2 i (Source es p is ts) (k,v') = let s' = go v' i (fromMaybe emptySource $  M.lookup k ts) in Source es p is  $ M.insert k s' ts
    g3 i (Source es p is ts) (k,v') = let s' = go v' i (fromMaybe emptySource $ MI.lookup k is) in Source es p (MI.insert k s' is) ts

loadYaml :: MonadIO m => FilePath -> SourcePackT m ()
loadYaml file = do
  v <- liftIO $ Y.decodeFileEither file
  modify $ \sp -> case v of
    Left  e -> addErr' (show e) sp
    Right a -> loadJSON (emptyReload $ pack file) a sp
