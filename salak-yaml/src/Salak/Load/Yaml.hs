{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TupleSections    #-}
module Salak.Load.Yaml where

import           Control.Monad.Catch
import           Control.Monad.Fail
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.ByteString              (ByteString)
import           Data.Conduit                 hiding (Source)
import           Data.Conduit.Lift
import qualified Data.HashMap.Strict          as HM
import qualified Data.Map                     as M
import           Data.Text                    (Text, pack)
import           Data.Text.Encoding           (decodeUtf8)
import qualified Data.Vector                  as V
import           Debug.Trace
import           Salak
import           Salak.Load
import           Text.Libyaml

loadYaml :: MonadIO m => FilePath -> SourcePackT m ()
loadYaml file = get >>= go >>= put
  where
    go sp = loadFile (defReload file $ loadYaml file) sp $ \i s -> 
      liftIO $ runConduitRes (decodeFileMarked file .| loadYAML i emptySource)

data YAML = YAML

instance HasLoad YAML where
  loaders _ = (, loadYaml) <$> ["yaml", "yml"]

loadYAML :: Monad m => Priority -> Source -> ConduitM MarkedEvent o m Source
loadYAML i s = trace (show s) $ await >>= maybe (return s) (\e -> trace (show $ yamlEvent e) $ go e)
  where
    go (MarkedEvent (EventScalar a _ _ _) _ _) = return (insertSource (VStr i $ decodeUtf8 a) s)
    go (MarkedEvent EventSequenceStart{}  _ _) = goSeq 0 s
    go (MarkedEvent EventSequenceEnd      _ _) = return emptySource
    go (MarkedEvent EventMappingStart{}   _ _) = goMap s
    go _ = loadYAML i s
    goSeq j s = do
      s' <- loadYAML i emptySource
      if nullSource s'
        then return s
        else updateSource (SNum j) (\_ -> return s') s >>= goSeq (j+1)
    goMap s = do
      v <- await
      case v of
        Nothing -> return s
        Just (MarkedEvent (EventScalar a _ _ _) _ _) -> 
          updateSources (simpleSelectors $ decodeUtf8 a) (loadYAML i) s >>= goMap
        _ -> return s






