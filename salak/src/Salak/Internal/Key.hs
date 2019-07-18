{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Salak.Internal.Key(
    Key(..)
  , Keys(..)
  , simpleKeys
  , ToKeys(..)
  , isNum
  ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Coerce          (coerce)
import           Data.Hashable
import           Data.List            (intercalate)
import           Data.Text            (Text)
import qualified Data.Text            as T

data Key
  = KT !Text
  | KI !Int
  deriving Eq

instance Ord Key where
  compare (KT a) (KT b) = compare a b
  compare (KI a) (KI b) = compare a b
  compare (KI _) _      = LT
  compare _      _      = GT

newtype Keys = Keys { unKeys :: [Key] } deriving Eq

instance Show Keys where
  show ks = toKey (coerce ks)
    where
      toKey = intercalate "." . go
      go (a@(KT _):cs) = let (b,c) = break isStr cs in (show a ++ concat (show <$> b)) : go c
      go (a:cs)          = show a : go cs
      go []              = []

isStr :: Key -> Bool
isStr (KT _) = True
isStr _      = False

isNum :: Key -> Bool
isNum (KI _) = True
isNum _      = False

instance Hashable Key where
  hash (KT a) = hash a
  hash (KI a) = hash a
  hashWithSalt i (KT a) = hashWithSalt i a
  hashWithSalt i (KI a) = hashWithSalt i a

instance Show Key where
  show (KT x) = T.unpack x
  show (KI i) = "[" ++ show i ++ "]"

simpleKeys :: Text -> [Key]
simpleKeys as = fmap KT $ filter (not.T.null) $ T.splitOn "." as

exprs :: Parser [Key]
exprs = concat <$> ( (expr <|> return []) `sepBy` char '.')

sName :: Parser Key
sName = KT . T.pack <$> do
    a <- choice [letter, digit]
    b <- many' (choice [letter, digit, char '-',  char '_'])
    return (a:b)

sNum :: Parser Key
sNum = KI <$> paren decimal
  where
    paren e = do
      _  <- char '['
      ex <- e
      _  <- char ']'
      return ex

-- xx
-- xx.xx
-- xx.xx[0]
-- xx.xx[1].xx
expr :: Parser [Key]
expr = (:) <$> sName <*> many' sNum

class ToKeys a where
  toKeys :: a -> Either String Keys

instance ToKeys Keys where
  toKeys = Right

instance ToKeys Text where
  toKeys = fmap Keys . selectors
    where
      selectors = go . parse exprs . flip T.snoc '\n'
      go (Done i r) = if i /= "\n" then Left $ "uncomplete parse" ++ T.unpack i else Right r
      go a          = Left (show a)

instance ToKeys String where
  toKeys = toKeys . T.pack
