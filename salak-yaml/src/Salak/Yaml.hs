{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- |
-- Module:      Salak.Yaml
-- Copyright:   (c) 2019 Daniel YU
-- License:     BSD3
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Yaml support for "Salak". Yaml alias and anchor is not supported.
--
module Salak.Yaml(
    YAML(..)
  , loadYaml
  ) where

import           Control.Exception      (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Conduit           hiding (Source)
import           Data.Text.Encoding     (decodeUtf8)
import           Salak
import           Salak.Load
import           Text.Libyaml

-- | Load Yaml
loadYaml :: MonadIO m => FilePath -> LoadSalakT m ()
loadYaml file = loadFile file $ \i s -> liftIO $ runConduitRes (decodeFileMarked file .| loadYAML i s)

-- | YAML notation for `loadYaml`
data YAML = YAML

instance HasLoad YAML where
  loaders _ = (, loadYaml) <$> ["yaml", "yml"]

loadYAML :: MonadIO m => Priority -> Source -> ConduitM MarkedEvent o m Source
loadYAML i s = await >>= maybe (return s) go
  where
    go (MarkedEvent (EventScalar a _ _ _) _ _) = return (insertSource (newVStr (decodeUtf8 a) i) s)
    go (MarkedEvent EventSequenceStart{}  _ _) = goSeq 0 s
    go (MarkedEvent EventSequenceEnd      _ _) = return emptySource
    go (MarkedEvent EventMappingStart{}  _ ee) = goMap ee s
    go (MarkedEvent (EventAlias a)       _ ee) = ge ee $ "alias " ++ a ++ " not supported by salak"
    go _ = loadYAML i s
    goSeq j s1 = do
      s' <- loadYAML i emptySource
      if nullSource s'
        then return s1
        else updateSource (SNum j) (\_ -> return s') s1 >>= goSeq (j+1)
    goMap ee s1 = do
      v <- await
      case v of
        Nothing -> ge ee "suppose to have data"
        Just (MarkedEvent (EventScalar a _ _ _) _ ee') ->
          updateSources (simpleSelectors $ decodeUtf8 a) (loadYAML i) s1 >>= goMap ee'
        Just (MarkedEvent EventMappingEnd _ _) -> return s1
        Just e -> ge (yamlStartMark e) ("suppose scalar and mapping end, but is " ++ show (yamlEvent e))
    ge YamlMark{..} e = liftIO $ throwIO $ YamlException $ "(" ++ show yamlLine ++ "," ++ show yamlColumn ++ ")" ++ e






