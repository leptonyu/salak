{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Salak.Internal.Key(
    Key(..)
  , Keys(..)
  , mempty
  , simpleKeys
  , singletonKey
  , fromKeys
  , toKeyList
  , ToKeys(..)
  , isNum
  , isStr
  , keyExpr
  , Parser
  ) where

import qualified Data.DList                 as D
import           Data.Hashable
import           Data.List                  (intercalate)
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup             hiding (option)
#endif

type Parser = Parsec Void Text

data Key
  = KT !Text
  | KI !Int
  deriving Eq

instance Ord Key where
  compare (KT a) (KT b) = compare a b
  compare (KI a) (KI b) = compare a b
  compare (KI _) _      = LT
  compare _      _      = GT

newtype Keys = Keys { unKeys :: D.DList Key } deriving (Eq, Ord)

emptyKey :: Keys
emptyKey = Keys D.empty

singletonKey :: Key -> Keys
singletonKey k = fromKeys [k]

fromKeys :: [Key] -> Keys
fromKeys = Keys . D.fromList

toKeyList :: Keys -> [Key]
toKeyList = D.toList . unKeys

instance Semigroup Keys where
  (Keys a) <> (Keys b) = Keys $ a <> b

instance Monoid Keys where
  mempty = emptyKey
  mappend = (<>)

instance Show Keys where
  show = intercalate "." . go . toKeyList
    where
      {-# INLINE go #-}
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

simpleKeys :: Text -> Keys
simpleKeys = fromKeys . fmap KT . filter (not.T.null) . T.splitOn "."

keyExpr :: Parser [Key]
keyExpr = concat <$> (option [] expr `sepBy` char '.')
  where
    -- xx
    -- xx.xx
    -- xx.xx[0]
    -- xx.xx[1].xx
    {-# INLINE expr #-}
    expr :: Parser [Key]
    expr = (:) <$> sName <*> many sNum

    {-# INLINE sName #-}
    sName :: Parser Key
    sName = KT . T.pack <$> some (alphaNumChar <|> char '-' <|> char '_')

    {-# INLINE sNum #-}
    sNum :: Parser Key
    sNum = do
      _  <- char '['
      ex <- decimal
      _  <- char ']'
      return $ KI ex

class ToKeys a where
  toKeys :: a -> Either String Keys

instance IsString Keys where
  fromString key = case toKeys key of
    Left  _ -> singletonKey (KT $ T.pack key)
    Right k -> k

instance ToKeys Keys where
  toKeys = Right

instance ToKeys Text where
  -- toKeys = Right . simpleKeys
  toKeys k = case fmap fromKeys (parse keyExpr "" k) of
    Left  e -> Left (show e)
    Right x -> Right x

instance ToKeys String where
  toKeys = toKeys . T.pack
