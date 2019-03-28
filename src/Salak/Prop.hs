{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Salak.Prop where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Int
import qualified Data.IntMap.Strict   as MI
import qualified Data.PQueue.Min      as Q
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Word
import           GHC.Exts
import           GHC.Generics         hiding (Selector)
import qualified GHC.Generics         as G
import           Salak.Types
import           Text.Read            (readMaybe)

data PResult a
  = O [Selector] a
  | N [Selector]
  | F [Selector] String
  deriving (Eq, Show, Functor)

instance Applicative PResult where
  pure = O []
  (O s f) <*> (O _ a) = O s (f a)
  (F s e) <*> _       = F s e
  _       <*> (F s e) = F s e
  (N s)   <*> _       = N s
  _       <*> (N s)   = N s

instance Alternative PResult where
  empty = N []
  (O s f) <|> _ = O s f
  _       <|> x = x

instance Monad PResult where
  return = pure
  (O _ a) >>= f = f a
  (N s  ) >>= _ = N s
  (F s e) >>= _ = F s e

type Prop a = ReaderT SourcePack PResult a

infixl 5 .?=
(.?=) :: Prop a -> a -> Prop a
(.?=) a b = a <|> pure b

instance FromProp a => IsString (Prop a) where
  fromString = readSelect . T.pack

class FromProp a where
  fromProp :: Prop a
  default fromProp :: (Generic a, GFromProp (Rep a)) => Prop a
  fromProp = fmap to gFromProp

class GFromProp f where
  gFromProp :: Prop (f a)

instance {-# OVERLAPPABLE #-} (Constructor c, GFromProp a) => GFromProp (M1 C c a) where
    gFromProp
      | conIsRecord m = fmap M1 gFromProp
      | otherwise     = fmap M1 $ gEnum $ T.pack (conName m)
      where m = undefined :: t c a x

gEnum va = do
  o <- gFromProp
  readPrimitive $ \ss v -> case v of
    VStr _ x -> if x /= va then N ss else O ss o
    _        -> N ss

instance {-# OVERLAPPABLE #-} (G.Selector s, GFromProp a) => GFromProp (M1 S s a) where
  gFromProp = local go $ M1 <$> gFromProp
    where
      go sp = select sp (STxt $ T.pack $ selName (undefined :: t s a p))

instance {-# OVERLAPPABLE #-} GFromProp a => GFromProp (M1 D i a) where
  gFromProp = M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} (FromProp a) => GFromProp (K1 i a) where
    gFromProp = fmap K1 fromProp

instance GFromProp U1 where
  gFromProp = pure U1

instance {-# OVERLAPPABLE #-} (GFromProp a, GFromProp b) => GFromProp (a:*:b) where
  gFromProp = (:*:) <$> gFromProp <*> gFromProp

instance {-# OVERLAPPABLE #-} (GFromProp a, GFromProp b) => GFromProp (a:+:b) where
  gFromProp = fmap L1 gFromProp <|> fmap R1 gFromProp

instance FromProp a => FromProp (Maybe a) where
  fromProp = do
    sp <- ask
    lift $ case runReaderT (fromProp :: Prop a) sp of
      O s a -> O s $ Just a
      N s   -> O s Nothing
      F s e -> F s e

instance {-# OVERLAPPABLE #-} FromProp a => FromProp [a] where
  fromProp = do
    SourcePack ss i (Source _ _ is _) it <- ask
    foldM (go ss i it) [] $ MI.toList is
    where
      go xx x xt as (ix,s) = do
        a <- lift $ runReaderT fromProp (SourcePack (SNum ix:xx) x s xt)
        return (a:as)

-- | ReadPrimitive value
readPrimitive :: ([Selector] -> Value -> PResult a) -> Prop a
readPrimitive f = do
  SourcePack ss _ (Source _ q _ _) _ <- ask
  case Q.getMin q of
    Just v -> lift $ f ss v
    _      -> lift $ N ss

err :: String -> Prop a
err e = do
  SourcePack ss _ _ _ <- ask
  lift $ F ss e

-- | Parse value
readSelect :: FromProp a => Text -> Prop a
readSelect key = case selectors key of
  Left  e -> err e
  Right s -> local (\sp -> foldl select sp s) fromProp

search :: FromProp a => Text -> SourcePack -> Either String a
search key sp = case runReaderT (readSelect key) sp of
  O _ x -> Right x
  N s   -> Left $ "key " ++ toKey s ++ " not found"
  F s e -> Left $ "key " ++ toKey s ++ " : " ++ e

instance FromProp Bool where
  fromProp = readPrimitive go
    where
      go s (VBool _ x) = O s x
      go s (VNum  _ _) = F s "number cannot be bool"
      go s (VStr  _ x) = case T.toLower x of
        "true"  -> O s True
        "yes"   -> O s True
        "false" -> O s False
        "no"    -> O s False
        _       -> F s "string convert bool failed"

instance FromProp Text where
  fromProp = readPrimitive go
    where
      go s (VStr  _ x) = O s x
      go s (VBool _ _) = F s "boolean cannot be string"
      go s (VNum  _ _) = F s "number cannot be string"

instance FromProp TL.Text where
  fromProp = TL.fromStrict <$> fromProp

instance FromProp String where
  fromProp = T.unpack <$> fromProp

instance FromProp Scientific where
  fromProp = readPrimitive go
    where
      go s (VStr  _ x) = case readMaybe $ T.unpack x of
        Just v -> O s v
        _      -> F s "string convert number failed"
      go s (VNum  _ x) = O s x
      go s (VBool _ _) = F s "boolean cannot be number"

instance FromProp Float where
  fromProp = toRealFloat <$> fromProp

instance FromProp Double where
  fromProp = toRealFloat <$> fromProp

instance FromProp Int where
  fromProp = fromProp >>= toNum

instance FromProp Int8 where
  fromProp = fromProp >>= toNum

instance FromProp Int16 where
  fromProp = fromProp >>= toNum

instance FromProp Int32 where
  fromProp = fromProp >>= toNum

instance FromProp Int64 where
  fromProp = fromProp >>= toNum

instance FromProp Word where
  fromProp = fromProp >>= toNum

instance FromProp Word8 where
  fromProp = fromProp >>= toNum

instance FromProp Word16 where
  fromProp = fromProp >>= toNum

instance FromProp Word32 where
  fromProp = fromProp >>= toNum

instance FromProp Word64 where
  fromProp = fromProp >>= toNum

toNum :: (Integral i, Bounded i) => Scientific -> Prop i
toNum s = case toBoundedInteger s of
  Just v -> return v
  _      -> err "scientific number doesn't fit in the target representation"
