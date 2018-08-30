module Data.Salak.CommandLine where

import           Data.Char
import           Data.Maybe
import           Data.Salak.Property
import           System.Environment

type ParseCommandLine = [String] -> IO [(String,Property)]

defaultParseCommandLine :: ParseCommandLine
defaultParseCommandLine = return . mapMaybe go
  where
    go ('-':'-':as) = case break (=='=') as of
      (a,'=':b) -> Just (a,PStr b)
      _         -> Nothing
    go _ = Nothing

makePropertiesFromCommandLine :: ParseCommandLine -> Properties -> IO Properties
makePropertiesFromCommandLine parser p = getArgs >>= (\a -> makePropertiesFromCommandLine' a parser p)

makePropertiesFromCommandLine' :: [String] -> ParseCommandLine -> Properties -> IO Properties
makePropertiesFromCommandLine' args parser p = do
  v <- parser args
  return $ makeProperties v p
