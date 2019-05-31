{-# LANGUAGE OverloadedStrings #-}
module Salak.Load.Env where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe
import qualified Data.Text              as T
import           Salak.Types
import           Salak.Types.Value
import           System.Environment

-- | Load environment variables into `SourcePack`
loadEnv :: MonadIO m => LoadSalakT m ()
loadEnv = do
  args <- liftIO getEnvironment
  loadOnce "environment" args go
  where
    go p (k,v) = return (g2 k, newVStr (T.pack v) p)
    g2 = T.toLower . T.pack . map (\c -> if c == '_' then '.' else c)

-- | Convert arguments to properties
type ParseCommandLine = [String] -> IO [(T.Text,Priority -> Value)]

-- | Default way to parse command line arguments
defaultParseCommandLine :: ParseCommandLine
defaultParseCommandLine = return . mapMaybe go
  where
    go ('-':'-':as) = case break (=='=') as of
      (a,'=':b) -> Just (T.pack a, newVStr (T.pack b))
      _         -> Nothing
    go _ = Nothing

-- | Load command line arguments into `SourcePack`
loadCommandLine :: MonadIO m => ParseCommandLine -> LoadSalakT m ()
loadCommandLine pcl = do
  args <- liftIO $ getArgs >>= pcl
  loadOnce "commandline" args go
  where
    go i (k,fv) = return (k, fv i)
