{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module Salak.Internal.Prop where

import qualified Control.Applicative     as A
import           Control.Monad
import qualified Control.Monad.Identity  as MI
import qualified Control.Monad.Reader    as MR
import qualified Control.Monad.State     as MS
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Default
import qualified Data.HashMap.Strict     as HM
import           Data.Int
import           Data.List               (sortBy)
import           Data.Menshen
import           Data.Scientific
import           Data.Semigroup
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TB
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TBL
import           Data.Word
import           Foreign.C
import           GHC.Exts
import           GHC.Generics
import           Salak.Internal.Key
import           Salak.Internal.Trie
import           Salak.Internal.Val
import qualified Salak.Trie              as TR
import           Text.Read               (readMaybe)

data PResult a
  = O [Key] a      -- ^ Succeed value
  | N [Key]        -- ^ Empty value
  | F [Key] String -- ^ Fail value
  deriving (Eq, Show, Functor)

instance Applicative PResult where
  pure = O []
  (O s f) <*> (O _ a) = O s (f a)
  (F s e) <*> _       = F s e
  _       <*> (F s e) = F s e
  (N s)   <*> _       = N s
  _       <*> (N s)   = N s

instance A.Alternative PResult where
  empty = N []
  (O s f) <|> _ = O s f
  _       <|> x = x

instance Monad PResult where
  return = pure
  (O _ a) >>= f = f a
  (N s  ) >>= _ = N s
  (F s e) >>= _ = F s e

-- | Optional value.
infixl 5 .?=
(.?=) :: A.Alternative f => f a -> a -> f a
(.?=) a b = a A.<|> pure b

-- | Default value.
infixl 5 .?:
(.?:) :: (A.Alternative f, Default b) => f a -> (b -> a) -> f a
(.?:) fa b = fa .?= b def

newtype PropT m a = Prop { unProp :: MR.ReaderT ([Key], Source) m a }
  deriving (Functor, Applicative, Monad, MS.MonadTrans, A.Alternative)

-- | Monad used to parse properties to destination type.
type Prop = PropT PResult

instance HasValid Prop where
  invalid = err . toI18n

instance FromProp a => IsString (Prop a) where
  fromString k = Prop $ do
    (p, t) <- MR.ask
    case search k t of
      Left  e      -> MR.lift $ F p e
      Right (ks,v) -> MR.withReaderT (const (p ++ unKeys ks,v)) (unProp fromProp)

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

gEnum :: GFromProp f => Text -> PropT PResult (f a)
gEnum va = do
  o <- gFromProp
  readPrimitive $ \ss v -> case v of
    VT x -> if x /= va then N ss else O ss o
    _    -> N ss

instance {-# OVERLAPPABLE #-} (Selector s, GFromProp a) => GFromProp (M1 S s a) where
  gFromProp = Prop $ do
    let k = KT $ T.pack $ selName (undefined :: t s a p)
    MR.withReaderT (\(ks,t) -> (k:ks,search1 t k)) $ unProp $ M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} GFromProp a => GFromProp (M1 D i a) where
  gFromProp = M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} (FromProp a) => GFromProp (K1 i a) where
    gFromProp = fmap K1 fromProp

instance GFromProp U1 where
  gFromProp = pure U1

instance {-# OVERLAPPABLE #-} (GFromProp a, GFromProp b) => GFromProp (a:*:b) where
  gFromProp = (:*:) <$> gFromProp <*> gFromProp

instance {-# OVERLAPPABLE #-} (GFromProp a, GFromProp b) => GFromProp (a:+:b) where
  gFromProp = fmap L1 gFromProp A.<|> fmap R1 gFromProp

runProp :: ([Key], Source) -> PropT m a -> m a
runProp sp a = MR.runReaderT (unProp a) sp

instance FromProp a => FromProp (Maybe a) where
  fromProp = Prop $ do
    kt <- MR.ask
    MR.lift $ case runProp kt (fromProp :: Prop a) of
      O s a -> O s $ Just a
      N s   -> O s Nothing
      F s e -> F s e

instance FromProp a => FromProp (Either String a) where
  fromProp = Prop $ do
    kt <- MR.ask
    MR.lift $ case runProp kt (fromProp :: Prop a) of
      O s a -> O s $ Right a
      N s   -> O s $ Left $ "null value " <> show (Keys s)
      F s e -> O s $ Left e

instance {-# OVERLAPPABLE #-} FromProp a => FromProp [a] where
  fromProp = Prop $ do
    (k,TR.Trie _ m) <- MR.ask
    MR.lift $ foldM (go k) [] $ sortBy g2 $ filter (isNum.fst) $ HM.toList m
    where
      go ks vs (k,t) = do
        v  <- runProp (k:ks, t) (fromProp :: Prop a)
        return (v:vs)
      g2 (a,_) (b,_) = compare b a

instance FromProp a => FromProp (MI.Identity a) where
  fromProp = MI.Identity <$> fromProp

instance (FromProp a, FromProp b) => FromProp (a,b) where
  fromProp = (,) <$> fromProp <*> fromProp

instance (FromProp a, FromProp b, FromProp c) => FromProp (a,b,c) where
  fromProp = (,,) <$> fromProp <*> fromProp <*> fromProp

instance (FromProp a, FromProp b, FromProp c, FromProp d) => FromProp (a,b,c,d) where
  fromProp = (,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp a, FromProp b, FromProp c, FromProp d, FromProp e) => FromProp (a,b,c,d,e) where
  fromProp = (,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp a, FromProp b, FromProp c, FromProp d, FromProp e, FromProp f) => FromProp (a,b,c,d,e,f) where
  fromProp = (,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp a, FromProp b, FromProp c, FromProp d, FromProp e, FromProp f, FromProp g) => FromProp (a,b,c,d,e,f,g) where
  fromProp = (,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp a, FromProp b, FromProp c, FromProp d, FromProp e, FromProp f, FromProp g, FromProp h) => FromProp (a,b,c,d,e,f,g,h) where
  fromProp = (,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp a, FromProp b, FromProp c, FromProp d, FromProp e, FromProp f, FromProp g, FromProp h, FromProp i) => FromProp (a,b,c,d,e,f,g,h,i) where
  fromProp = (,,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp


instance FromProp a => FromProp (Min a) where
  fromProp = Min <$> fromProp

instance FromProp a => FromProp (Max a) where
  fromProp = Max <$> fromProp

instance FromProp a => FromProp (First a) where
  fromProp = First <$> fromProp

instance FromProp a => FromProp (Last a) where
  fromProp = Last <$> fromProp

instance FromProp a => FromProp (Dual a) where
  fromProp = Dual <$> fromProp

instance FromProp a => FromProp (Sum a) where
  fromProp = Sum <$> fromProp

instance FromProp a => FromProp (Product a) where
  fromProp = Product <$> fromProp

instance FromProp a => FromProp (Option a) where
  fromProp = Option <$> fromProp

-- | Parse primitive value from `Value`
readPrimitive :: ([Key] -> Value -> PResult a) -> Prop a
readPrimitive f = Prop $ do
  (k,TR.Trie v _) <- MR.ask
  MR.lift $ case v >>= getVal of
    Just x -> f k x
    _      -> N k

class FromEnumProp a where
  fromEnumProp :: Text -> Either String a
  {-# MINIMAL fromEnumProp #-}


instance FromProp Bool where
  fromProp = readPrimitive go
    where
      go s (VB x) = O s x
      go s (VT x) = case T.toLower x of
        "true"  -> O s True
        "yes"   -> O s True
        "false" -> O s False
        "no"    -> O s False
        _       -> F s "string convert bool failed"
      go s x             = F s $ getType x ++ " cannot be bool"

instance FromProp Text where
  fromProp = readPrimitive go
    where
      go s (VT x) = O s x
      go s x      = O s $ T.pack $ snd $ typeOfV x

instance FromProp TL.Text where
  fromProp = TL.fromStrict <$> fromProp

instance FromProp B.ByteString where
  fromProp = TB.encodeUtf8 <$> fromProp

instance FromProp BL.ByteString where
  fromProp = TBL.encodeUtf8 <$> fromProp

instance FromProp String where
  fromProp = T.unpack <$> fromProp

instance FromProp Scientific where
  fromProp = readPrimitive go
    where
      go s (VT x) = case readMaybe $ T.unpack x of
        Just v -> O s v
        _      -> F s "string convert number failed"
      go s (VI x) = O s x
      go s x      = F s $ getType x ++ " cannot be number"

instance FromProp Float where
  fromProp = toRealFloat <$> fromProp

instance FromProp Double where
  fromProp = toRealFloat <$> fromProp

instance FromProp Integer where
  fromProp = toInteger <$> (fromProp :: Prop Int)

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

instance FromProp CBool where
  fromProp = do
    b <- fromProp
    return $ if b then 1 else 0

instance FromProp CShort where
  fromProp = CShort <$> fromProp

instance FromProp CUShort where
  fromProp = CUShort <$> fromProp

instance FromProp CInt where
  fromProp = CInt <$> fromProp

instance FromProp CUInt where
  fromProp = CUInt <$> fromProp

instance FromProp CLong where
  fromProp = CLong <$> fromProp

instance FromProp CULong where
  fromProp = CULong <$> fromProp

instance FromProp CLLong where
  fromProp = CLLong <$> fromProp

instance FromProp CULLong where
  fromProp = CULLong <$> fromProp

instance FromProp CFloat where
  fromProp = CFloat <$> fromProp

instance FromProp CDouble where
  fromProp = CDouble <$> fromProp


err :: String -> Prop a
err e = Prop $ do
  (k, _) <- MR.ask
  MR.lift $ F k e
