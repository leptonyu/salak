module Data.Salak.CommandLine where

import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Maybe
import           Data.Salak.Types
import           Data.Text              (pack)
import           System.Environment

-- | CommandLine parser. Parse command line into property key values.
type ParseCommandLine = [String] -> IO [(String,Property)]

-- | Default command line parsers.
--   Use format:
--
-- > --KEY=VALUE
--
-- For example:
--
-- > --salak.config.name=test.yml => ("salak.config.name", PStr "test.yml")
--
defaultParseCommandLine :: ParseCommandLine
defaultParseCommandLine = return . mapMaybe go
  where
    go ('-':'-':as) = case break (=='=') as of
      (a,'=':b) -> Just (a,PStr $ pack b)
      _         -> Nothing
    go _ = Nothing

-- | Load `Properties` from 'CommandLine'
makePropertiesFromCommandLine :: ParseCommandLine -> Properties -> IO Properties
makePropertiesFromCommandLine parser p = getArgs >>= (\a -> makePropertiesFromCommandLine' a parser p)

makePropertiesFromCommandLine' :: [String] -> ParseCommandLine -> Properties -> IO Properties
makePropertiesFromCommandLine' args parser p = do
  v <- parser args
  return $ makeProperties (fmap (\(a,b) -> (pack a,b)) v) p


-- | Load Properties from CommandLine
--
-- @since 0.2.2
loadCommandLine :: MonadIO m => ParseCommandLine -> LoadProperties m ()
loadCommandLine pcl = do
  p <- get
  q <- liftIO $ makePropertiesFromCommandLine pcl p
  put q
