{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Salak.Internal.Prop where

import qualified Control.Applicative     as A
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
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

class Monad m => FromProp m a where
  fromProp :: Prop m a
  default fromProp :: (Generic a, GFromProp m (Rep a)) => Prop m a
  fromProp = fmap to gFromProp

newtype Prop m a
  = Prop { unProp :: ReaderT SourcePack m a }
  deriving (Functor, Applicative, Monad, MonadReader SourcePack, MonadTrans)

instance MonadCatch m => A.Alternative (Prop m) where
  empty = notFound
  a <|> b = do
    v <- try a
    case v of
      Right x                   -> return x
      Left (_ :: SomeException) -> b

class HasSalak m where
  require :: FromProp m a => Text -> m a

instance (MonadThrow m, MonadSalak m) => HasSalak m where
  require ks = do
    sp@SourcePack{..} <- askSalak
    case search ks source of
      Left  e     -> throwM $ PropException e
      Right (k,t) -> runProp sp { source = t, pref = pref ++ unKeys k} fromProp

instance Monad m => MonadSalak (Prop m) where
  askSalak = Prop ask

instance MonadThrow m => MonadThrow (Prop m) where
  throwM = Prop . throwM

instance MonadCatch m => MonadCatch (Prop m) where
  catch (Prop a) f = Prop $ do
    sp <- ask
    lift $ runReaderT a sp `catch` (\e -> runReaderT (unProp $ f e) sp)

runProp :: Monad m => SourcePack -> Prop m a ->  m a
runProp sp (Prop p) = runReaderT p sp

instance (Monad m, FromProp m a) => FromProp m (Maybe a) where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    if TR.empty == source
      then return Nothing
      else lift $ Just <$> runProp sp fromProp

instance (MonadCatch m, FromProp m a) => FromProp m (Either String a) where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    lift $ convertExp <$> try (runProp sp fromProp)

instance {-# OVERLAPPABLE #-} FromProp m a => FromProp m [a] where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    let TR.Trie _ m = source
    lift $ foldM (go sp) [] $ sortBy g2 $ filter (isNum.fst) $ HM.toList m
    where
      go s vs (k,t) = (:vs) <$> runProp s { pref = pref s ++ [k], source = t} fromProp
      g2 (a,_) (b,_) = compare b a

instance {-# OVERLAPPABLE #-} (MonadThrow m, MonadIO m, FromProp (Either SomeException) a) => FromProp m (IO a) where
  fromProp = Prop $ do
    sp   <- ask
    either throwM (go sp) $ runProp sp (fromProp :: Prop (Either SomeException) a)
    where
      go sp a = liftIO $ do
        aref <- newMVar a
        modifyMVar_ (qref sp) $ \f -> return $ \s -> do
          b  <- convertExp $ runProp sp {source = search2 s (pref sp), pref = pref sp} fromProp
          io <- f s
          return (swapMVar aref b >> io)
        return (readMVar aref)

convertExp :: Either SomeException a -> Either String a
convertExp = either (Left . readExp) Right
  where
    readExp e = case fromException e of
      Just (PropException x) -> x
      _                      -> show e

newtype PropException = PropException String deriving Show

instance Exception PropException

instance (MonadThrow m, FromProp m a) => IsString (Prop m a) where
  fromString ks = do
    sp@SourcePack{..} <- askSalak
    lift $ case search ks source of
      Left  e     -> throwM $ PropException e
      Right (k,t) -> runProp sp { source = t, pref = pref ++ unKeys k} fromProp

notFound :: MonadThrow m => Prop m a
notFound = err "null value"

err :: MonadThrow m => String -> Prop m a
err e = do
  SourcePack{..} <- askSalak
  throwM $ PropException $ show (Keys pref) ++ ":" ++ e

-- | Optional value.
infixl 5 .?=
(.?=) :: A.Alternative f => f a -> a -> f a
(.?=) a b = a A.<|> pure b

-- | Default value.
infixl 5 .?:
(.?:) :: (A.Alternative f, Default b) => f a -> (b -> a) -> f a
(.?:) fa b = fa .?= b def

instance MonadThrow m => HasValid (Prop m) where
  invalid = err . toI18n

-- | Parse primitive value from `Value`
readPrimitive :: MonadThrow m => (Value -> Either String a) -> Prop m a
readPrimitive f = do
  SourcePack{..} <- askSalak
  let TR.Trie v _ = source
  case f <$> (v >>= getVal) of
    Just (Left e)  -> err e
    Just (Right a) -> return a
    _              -> notFound

readEnum :: MonadThrow m => (Text -> Either String a) -> Prop m a
readEnum = readPrimitive . go
  where
    go f (VT t) = f t
    go _ x      = Left $ fst (typeOfV x) ++ " cannot convert to enum"

class Monad m => GFromProp m f where
  gFromProp :: Prop m (f a)

instance {-# OVERLAPPABLE #-} (MonadThrow m, Constructor c, GFromProp m a) => GFromProp m (M1 C c a) where
    gFromProp
      | conIsRecord m = fmap M1 gFromProp
      | otherwise     = fmap M1 $ gEnum $ T.pack (conName m)
      where m = undefined :: t c a x

gEnum :: (MonadThrow m, GFromProp m f) => Text -> Prop m (f a)
gEnum va = do
  o <- gFromProp
  readEnum $ \x -> if x==va then Right o else Left "enum invalid"

instance {-# OVERLAPPABLE #-} (Selector s, GFromProp m a) => GFromProp m(M1 S s a) where
  gFromProp = Prop $ do
    let k = KT $ T.pack $ selName (undefined :: t s a p)
    withReaderT (\s -> s { pref = pref s ++ [k], source = search1 (source s) k }) $ unProp $ M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} GFromProp m a => GFromProp m (M1 D i a) where
  gFromProp = M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} (FromProp m a) => GFromProp m (K1 i a) where
    gFromProp = fmap K1 fromProp

instance Monad m => GFromProp m U1 where
  gFromProp = pure U1

instance {-# OVERLAPPABLE #-} (GFromProp m a, GFromProp m b) => GFromProp m (a:*:b) where
  gFromProp = (:*:) <$> gFromProp <*> gFromProp

instance {-# OVERLAPPABLE #-} (MonadCatch m, GFromProp m a, GFromProp m b) => GFromProp m (a:+:b) where
  gFromProp = fmap L1 gFromProp A.<|> fmap R1 gFromProp

instance (Monad m, FromProp m a) => FromProp m (Identity a) where
  fromProp = Identity <$> fromProp

instance (Monad m, FromProp m a, FromProp m b) => FromProp m (a,b) where
  fromProp = (,) <$> fromProp <*> fromProp

instance (Monad m, FromProp m a, FromProp m b, FromProp m c) => FromProp m(a,b,c) where
  fromProp = (,,) <$> fromProp <*> fromProp <*> fromProp

instance (Monad m, FromProp m a, FromProp m b, FromProp m c, FromProp m d) => FromProp m(a,b,c,d) where
  fromProp = (,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (Monad m, FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e) => FromProp m(a,b,c,d,e) where
  fromProp = (,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (Monad m, FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f) => FromProp m(a,b,c,d,e,f) where
  fromProp = (,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (Monad m, FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g) => FromProp m(a,b,c,d,e,f,g) where
  fromProp = (,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (Monad m, FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g, FromProp m h) => FromProp m(a,b,c,d,e,f,g,h) where
  fromProp = (,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (Monad m, FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g, FromProp m h, FromProp m i) => FromProp m(a,b,c,d,e,f,g,h,i) where
  fromProp = (,,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp


instance (Monad m, FromProp m a) => FromProp m (Min a) where
  fromProp = Min <$> fromProp

instance (Monad m, FromProp m a) => FromProp m (Max a) where
  fromProp = Max <$> fromProp

instance (Monad m, FromProp m a) => FromProp m (First a) where
  fromProp = First <$> fromProp

instance (Monad m, FromProp m a) => FromProp m (Last a) where
  fromProp = Last <$> fromProp

instance (Monad m, FromProp m a) => FromProp m (Dual a) where
  fromProp = Dual <$> fromProp

instance (Monad m, FromProp m a) => FromProp m (Sum a) where
  fromProp = Sum <$> fromProp

instance (Monad m, FromProp m a) => FromProp m (Product a) where
  fromProp = Product <$> fromProp

instance (Monad m, FromProp m a) => FromProp m (Option a) where
  fromProp = Option <$> fromProp

instance MonadThrow m => FromProp m Bool where
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

instance MonadThrow m => FromProp m Text where
  fromProp = readPrimitive go
    where
      go (VT x) = Right x
      go x      = Right $ T.pack $ snd $ typeOfV x

instance MonadThrow m => FromProp m TL.Text where
  fromProp = TL.fromStrict <$> fromProp

instance MonadThrow m => FromProp m B.ByteString where
  fromProp = TB.encodeUtf8 <$> fromProp

instance MonadThrow m => FromProp m BL.ByteString where
  fromProp = TBL.encodeUtf8 <$> fromProp

instance MonadThrow m => FromProp m String where
  fromProp = T.unpack <$> fromProp

instance MonadThrow m => FromProp m Scientific where
  fromProp = readPrimitive go
    where
      go (VT x) = case readMaybe $ T.unpack x of
        Just v -> Right v
        _      -> Left  "string convert number failed"
      go (VI x) = Right x
      go x      = Left $ getType x ++ " cannot be number"

instance MonadThrow m => FromProp m Float where
  fromProp = toRealFloat <$> fromProp

instance MonadThrow m => FromProp m Double where
  fromProp = toRealFloat <$> fromProp

instance MonadThrow m => FromProp m Integer where
  fromProp = toInteger <$> (fromProp :: Prop m Int)

instance MonadThrow m => FromProp m Int where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Int8 where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Int16 where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Int32 where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Int64 where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Word where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Word8 where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Word16 where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Word32 where
  fromProp = fromProp >>= toNum

instance MonadThrow m => FromProp m Word64 where
  fromProp = fromProp >>= toNum

toNum :: (MonadThrow m, Integral i, Bounded i) => Scientific -> Prop m i
toNum s = case toBoundedInteger s of
  Just v -> return v
  _      -> err "scientific number doesn't fit in the target representation"

instance MonadThrow m => FromProp m CBool where
  fromProp = do
    b <- fromProp
    return $ if b then 1 else 0

instance MonadThrow m => FromProp m CShort where
  fromProp = CShort <$> fromProp

instance MonadThrow m => FromProp m CUShort where
  fromProp = CUShort <$> fromProp

instance MonadThrow m => FromProp m CInt where
  fromProp = CInt <$> fromProp

instance MonadThrow m => FromProp m CUInt where
  fromProp = CUInt <$> fromProp

instance MonadThrow m => FromProp m CLong where
  fromProp = CLong <$> fromProp

instance MonadThrow m => FromProp m CULong where
  fromProp = CULong <$> fromProp

instance MonadThrow m => FromProp m CLLong where
  fromProp = CLLong <$> fromProp

instance MonadThrow m => FromProp m CULLong where
  fromProp = CULLong <$> fromProp

instance MonadThrow m => FromProp m CFloat where
  fromProp = CFloat <$> fromProp

instance MonadThrow m => FromProp m CDouble where
  fromProp = CDouble <$> fromProp

