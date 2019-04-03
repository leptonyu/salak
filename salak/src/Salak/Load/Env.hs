{-# LANGUAGE OverloadedStrings #-}
module Salak.Load.Env where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Text              as T
import           Salak.Types
import           Salak.Types.Value
import           System.Environment

loadEnv :: MonadIO m => SourcePackT m ()
loadEnv = do
  args <- liftIO getEnvironment
  get >>= load (emptyReload "environment") args go >>= put
  where
    go p (k,v) = return (g2 k, VStr p $ T.pack v)
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
  get >>= load (emptyReload "commandline") args go >>= put
  where
    go i (k,fv) = return (k, fv i)
