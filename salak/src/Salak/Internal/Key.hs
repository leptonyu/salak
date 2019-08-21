module Salak.Internal.Key(
    Key(..)
  , Keys(..)
  , mempty
  , simpleKeys
  , singletonKey
  , fromKeys
  , toKeyList
  , showKey
  , ToKeys(..)
  , isNum
  , isStr
  , keyExpr
  , Parser
  , (<>)
  ) where

import qualified Data.DList                 as D
import           Data.Hashable
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
  {-# INLINE compare #-}
  compare (KT a) (KT b) = compare a b
  compare (KI a) (KI b) = compare a b
  compare (KI _) _      = LT
  compare _      _      = GT

newtype Keys = Keys { unKeys :: D.DList Key } deriving (Eq, Ord)

{-# INLINE emptyKey #-}
emptyKey :: Keys
emptyKey = Keys D.empty

{-# INLINE singletonKey #-}
singletonKey :: Key -> Keys
singletonKey k = fromKeys [k]

{-# INLINE fromKeys #-}
fromKeys :: [Key] -> Keys
fromKeys = Keys . D.fromList

{-# INLINE toKeyList #-}
toKeyList :: Keys -> [Key]
toKeyList = D.toList . unKeys

instance Semigroup Keys where
  {-# INLINE (<>) #-}
  (Keys a) <> (Keys b) = Keys $ a <> b

instance Monoid Keys where
  {-# INLINE mempty #-}
  mempty = emptyKey
  {-# INLINE mappend #-}
  mappend = (<>)

instance Show Keys where
  {-# INLINE show #-}
  show = T.unpack . showKey

{-# INLINE showKey #-}
showKey :: Keys -> Text
showKey = T.intercalate "." . go . toKeyList
  where
    {-# INLINE go #-}
    go (KT a : as) = let (b,cs) = break isStr as in a <> g2 b : go cs
    go (a:as)      = let (b,cs) = break isStr as in g2 (a:b)  : go cs
    go []          = []
    {-# INLINE g2 #-}
    g2 = T.concat . fmap g3
    {-# INLINE g3 #-}
    g3 (KI a) = "[" <> fromString (show a) <> "]"
    g3 (KT a) = a

    -- go (KI a) = "[" <> fromString (show a) <> "]"


isStr :: Key -> Bool
isStr (KT _) = True
isStr _      = False

isNum :: Key -> Bool
isNum (KI _) = True
isNum _      = False

instance Hashable Key where
  {-# INLINE hash #-}
  hash (KT a) = hash a
  hash (KI a) = hash a
  {-# INLINE hashWithSalt #-}
  hashWithSalt i (KT a) = hashWithSalt i a
  hashWithSalt i (KI a) = hashWithSalt i a

instance Show Key where
  {-# INLINE show #-}
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
  {-# INLINE fromString #-}
  fromString key = case toKeys key of
    Left  _ -> singletonKey (KT $ T.pack key)
    Right k -> k

instance ToKeys Keys where
  {-# INLINE toKeys #-}
  toKeys = Right

instance ToKeys Text where
  -- toKeys = Right . simpleKeys
  {-# INLINE toKeys #-}
  toKeys k = case fmap fromKeys (parse keyExpr "" k) of
    Left  e -> Left (errorBundlePretty e)
    Right x -> Right x

instance ToKeys String where
  {-# INLINE toKeys #-}
  toKeys = toKeys . T.pack
