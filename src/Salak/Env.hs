{-# LANGUAGE OverloadedStrings #-}
module Salak.Env where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Text              as T
import           Salak.Types
import           System.Environment

loadEnv :: MonadIO m => SourcePackT m ()
loadEnv = do
  args <- liftIO getEnvironment
  modify $ load (emptyReload "environment") args go
  where
    go p (k,v) = (g2 k, VStr p $ T.pack v)
    g2 = T.toLower . T.pack . map (\c -> if c == '_' then '.' else c)

type ParseCommandLine = [String] -> IO [(T.Text,Priority -> Value)]

defaultParseCommandLine :: ParseCommandLine
defaultParseCommandLine = return . mapMaybe go
  where
    go ('-':'-':as) = case break (=='=') as of
      (a,'=':b) -> Just (T.pack a, \p -> VStr p $ T.pack b)
      _         -> Nothing
    go _ = Nothing

loadCommandLine :: MonadIO m => ParseCommandLine -> SourcePackT m ()
loadCommandLine pcl = do
  args <- liftIO $ getArgs >>= pcl
  modify $ load (emptyReload "commandline") args go
  where
    go i (k,fv) = (k, fv i)
