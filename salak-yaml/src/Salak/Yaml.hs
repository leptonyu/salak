{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- |
-- Module:      Salak.Yaml
-- Copyright:   (c) 2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Yaml support for "Salak". Yaml alias and anchor is not supported.
--
module Salak.Yaml(
    YAML(..)
  , loadYaml
  , runSalakWithYaml
  ) where

import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Conduit           hiding (Source)
import           Data.Text.Encoding     (decodeUtf8)
import           Salak
import           Salak.Internal
import qualified Salak.Trie             as T
import           Text.Libyaml

runSalakWithYaml :: (MonadCatch m, MonadIO m) => FilePath -> RunSalakT m a -> m a
runSalakWithYaml name = runSalakWith name YAML

-- | Load Yaml
loadYaml :: FilePath -> LoadSalak ()
loadYaml file = loadTrie True file (\i -> runConduitRes (decodeFileMarked file .| loadYAML i T.empty))

-- | YAML notation for `loadYaml`
data YAML = YAML

instance HasLoad YAML where
  loaders _ = (, loadYaml) <$> ["yaml", "yml"]

loadYAML :: MonadIO m => Int -> TraceSource -> ConduitM MarkedEvent o m TraceSource
loadYAML i = start
  where
    {-# INLINE start #-}
    start ts = await >>= maybe (return ts) (go ts)

    {-# INLINE go #-}
    go _  (MarkedEvent (EventAlias a) _ ee)       = ge ee $ "alias " ++ a ++ " not supported by salak"
    go ts (MarkedEvent (EventScalar a _ _ _) _ _) = return $ setVal i a ts
    go ts (MarkedEvent EventSequenceStart{}  _ _) = goS 0 ts
    go ts (MarkedEvent EventMappingStart{}   _ _) = goM ts
    go ts  _                                      = start ts

    {-# INLINE goS #-}
    goS j ts = do
      v <- await
      case v of
        Nothing -> liftIO $ throwIO $ YamlException "unexpected end"
        Just (MarkedEvent EventSequenceEnd _ _) -> return ts
        Just e -> do
          val <- go T.empty e
          goS (j+1) (T.modify (KI j) (const val) ts)

    {-# INLINE goM #-}
    goM ts = do
      v <- await
      case v of
        Nothing -> liftIO $ throwIO $ YamlException "unexpected end"
        Just (MarkedEvent EventMappingEnd _ _) -> return ts
        Just (MarkedEvent (EventScalar a _ _ _) _ _) -> do
                val <- start T.empty
                goM $ T.modify' (const val) (simpleKeys $ decodeUtf8 a) ts
        Just e -> ge (yamlStartMark e) ("suppose scalar and mapping end, but is " ++ show (yamlEvent e))

    {-# INLINE ge #-}
    ge YamlMark{..} e = liftIO $ throwIO $ YamlException $ "(" ++ show yamlLine ++ "," ++ show yamlColumn ++ ")" ++ e

