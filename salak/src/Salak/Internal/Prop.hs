{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Salak.Internal.Prop where

import qualified Control.Applicative     as A
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Identity  (Identity (..))
import           Control.Monad.Reader
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
import           Salak.Internal.Source
import           Salak.Internal.Val
import qualified Salak.Trie              as TR
import           Text.Read               (readMaybe)

class FromPropT m a where
  fromPropT :: PropT m a

class FromProp a where
  fromProp :: PropT (Either String) a
  default fromProp :: (Generic a, GFromProp (Rep a)) => Prop a
  fromProp = fmap to gFromProp

newtype PropT m a
  = PropT { unPropT :: ReaderT SourcePack m a }
  deriving (Functor, Applicative, Monad, MonadTrans, A.Alternative)

type Prop = PropT (Either String)

class HasSalak m where
  require :: FromPropT m a => Text -> m a

instance (MonadThrow m, MonadSalak m) => HasSalak m where
  require ks = do
    sp@SourcePack{..} <- askSalak
    case search ks source of
      Left  e     -> throwM $ PropException e
      Right (k,t) -> runPropT sp { source = t, pref = pref ++ unKeys k} fromPropT

instance {-# OVERLAPPABLE #-} Monad m => MonadSalak (PropT m) where
  askSalak = PropT ask

instance MonadSalak Prop where
  askSalak = PropT ask

instance MonadThrow m => MonadThrow (PropT m) where
  throwM = PropT . throwM

runPropT :: Monad m => SourcePack -> PropT m a -> m a
runPropT sp (PropT p) = runReaderT p sp

instance FromProp a => FromProp (Maybe a) where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    if TR.empty == source
      then return Nothing
      else lift $ Just <$> runPropT sp fromProp

instance (FromProp a) => FromProp (Either String a) where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    return $ runPropT sp fromProp

instance {-# OVERLAPPABLE #-} FromProp a => FromProp [a] where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    let TR.Trie _ m = source
    lift $ foldM (go sp) [] $ sortBy g2 $ filter (isNum.fst) $ HM.toList m
    where
      go s vs (k,t) = (:vs) <$> runPropT s { pref = pref s ++ [k], source = t} fromProp
      g2 (a,_) (b,_) = compare b a


instance (MonadIO m, MonadThrow m, FromProp a) => FromPropT m (IO a) where
  fromPropT = PropT $ do
    sp   <- ask
    a    <- lift $ runPropT sp fromPropT
    aref <- liftIO $ newMVar a
    liftIO $ modifyMVar_ (qref sp) $ \f -> return $ \s -> do
      b  <- runPropT sp {source = s} fromProp
      io <- f s
      return (swapMVar aref b >> io)
    return (readMVar aref)

newtype PropException = PropException String deriving Show

instance Exception PropException

instance (MonadThrow m, FromProp a) => FromPropT m a where
  fromPropT = do
    sp <- askSalak
    case runPropT sp fromProp of
      Left  e -> throwM $ PropException e
      Right a -> return a

instance FromProp a => IsString (Prop a) where
  fromString ks = do
    sp@SourcePack{..} <- askSalak
    lift $ case search ks source of
      Left  e     -> Left e
      Right (k,t) -> runPropT sp { source = t, pref = pref ++ unKeys k} fromProp

notFound :: Prop a
notFound = err "null value"

err :: String -> Prop a
err e = do
  SourcePack{..} <- askSalak
  lift $ Left $ show pref ++ ":" ++ e

-- | Optional value.
infixl 5 .?=
(.?=) :: A.Alternative f => f a -> a -> f a
(.?=) a b = a A.<|> pure b

-- | Default value.
infixl 5 .?:
(.?:) :: (A.Alternative f, Default b) => f a -> (b -> a) -> f a
(.?:) fa b = fa .?= b def

instance HasValid Prop where
  invalid = err . toI18n

-- | Parse primitive value from `Value`
readPrimitive :: (Value -> Either String a) -> Prop a
readPrimitive f = do
  SourcePack{..} <- askSalak
  let TR.Trie v _ = source
  lift $ maybe (Left "null value") f $ v >>= getVal

readEnum :: (Text -> Either String a) -> Prop a
readEnum = readPrimitive . go
  where
    go f (VT t) = f t
    go _ x      = Left $ fst (typeOfV x) ++ " cannot convert to enum"


class GFromProp f where
  gFromProp :: Prop (f a)

instance {-# OVERLAPPABLE #-} (Constructor c, GFromProp a) => GFromProp (M1 C c a) where
    gFromProp
      | conIsRecord m = fmap M1 gFromProp
      | otherwise     = fmap M1 $ gEnum $ T.pack (conName m)
      where m = undefined :: t c a x

gEnum :: GFromProp f => Text -> Prop (f a)
gEnum va = do
  o <- gFromProp
  readEnum $ \x -> if x==va then Right o else Left "enum invalid"

instance {-# OVERLAPPABLE #-} (Selector s, GFromProp a) => GFromProp (M1 S s a) where
  gFromProp = PropT $ do
    let k = KT $ T.pack $ selName (undefined :: t s a p)
    withReaderT (\s -> s { pref = pref s ++ [k], source = search1 (source s) k }) $ unPropT $ M1 <$> gFromProp

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

instance FromProp a => FromProp (Identity a) where
  fromProp = Identity <$> fromProp

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

instance FromProp Bool where
  fromProp = readPrimitive go
    where
      go (VB x) = Right x
      go (VT x) = case T.toLower x of
        "true"  -> Right True
        "yes"   -> Right True
        "false" -> Right False
        "no"    -> Right False
        _       -> Left "string convert bool failed"
      go x      = Left $ getType x ++ " cannot be bool"

instance FromProp Text where
  fromProp = readPrimitive go
    where
      go (VT x) = Right x
      go x      = Right $ T.pack $ snd $ typeOfV x

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
      go (VT x) = case readMaybe $ T.unpack x of
        Just v -> Right v
        _      -> Left  "string convert number failed"
      go (VI x) = Right x
      go x      = Left $ getType x ++ " cannot be number"

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

