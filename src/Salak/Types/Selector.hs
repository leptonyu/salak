{-# LANGUAGE OverloadedStrings #-}
module Salak.Types.Selector where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.List            (intercalate)
import           Data.Text            (Text)
import qualified Data.Text            as T

data Selector
  = SStr !Text
  | SNum !Int
  deriving (Eq, Ord)

instance Show Selector where
  show (SStr x) = T.unpack x
  show (SNum i) = "[" ++ show i ++ "]"

toKey :: [Selector] -> String
toKey = intercalate "." . go . reverse
  where
    -- TODO: fix snum
    go (a@(SStr _):b@(SNum _):cs) = (show a ++ show b) : go cs
    go (a:bs)                     = show a : go bs
    go []                         = []

simpleSelectors :: Text -> [Selector]
simpleSelectors as = fmap SStr $ filter (not.T.null) $ T.splitOn "." as

selectors :: Text -> Either String [Selector]
selectors = go . parse exprs . flip T.snoc '\n'
  where
    go (Done i r) = if i /= "\n" then Left $ "uncomplete parse" ++ T.unpack i else Right r
    go a          = Left (show a)

exprs :: Parser [Selector]
exprs = concat <$> ( (expr <|> return []) `sepBy` char '.')

-- xx
-- xx.xx
-- xx.xx[0]
-- xx.xx[1].xx
expr :: Parser [Selector]
expr = do
  name <- T.pack <$> do
    a <- choice [letter, digit]
    b <- many' (choice [letter, digit, char '-',  char '_'])
    return (a:b)
  ds   <- many' (paren decimal)
  return $ SStr name : (SNum <$> ds)
  where
    paren e = do
      _  <- char '['
      ex <- e
      _  <- char ']'
      return ex