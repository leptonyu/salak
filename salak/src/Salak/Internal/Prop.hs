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
import           Data.Fixed
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HM
import           Data.Int
import           Data.List               (sortBy)
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Menshen
import           Data.Scientific
import           Data.Semigroup
import           Data.Text               (Text, unpack)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TB
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TBL
import           Data.Time
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
  = Prop { unProp :: ReaderT SourcePack (ExceptT SomeException m) a }
  deriving (Functor, Applicative, Monad, MonadReader SourcePack, MonadIO)

instance MonadTrans Prop where
  lift = Prop . lift . lift

instance Monad m => A.Alternative (Prop m) where
  empty = notFound
  a <|> b = do
    v <- try a
    case v of
      Right x                   -> return x
      Left (_ :: SomeException) -> b

-- | Core type class of salak, which provide function to parse properties.
class Monad m => MonadSalak m where
  -- | Monad has the ability to get a `SourcePack` instance.
  askSalak :: m SourcePack

  -- | Get reload action which used for reload profiles
  askReload :: m (IO ReloadResult)
  askReload = reload <$> askSalak

  setLogF :: MonadIO m => (String -> IO ()) -> m ()
  setLogF f = do
    SourcePack{..} <- askSalak
    liftIO $ void $ swapMVar lref f

  logSalak :: MonadIO m => String -> m ()
  logSalak msg = do
    SourcePack{..} <- askSalak
    liftIO $ do
      f <- readMVar lref
      f msg

  -- | Parse properties using `FromProp`. For example:
  --
  -- > a :: Bool              <- require "bool.key"
  -- > b :: Maybe Int         <- require "int.optional.key"
  -- > c :: Either String Int <- require "int.error.key"
  -- > d :: IO Int            <- require "int.reloadable.key"
  --
  -- `require` supports parse `IO` values, which actually wrap a 'MVar' variable and can be reseted by reloading configurations.
  -- Normal value will not be affected by reloading configurations.
  require :: (MonadThrow m, FromProp m a) => Text -> m a
  require ks = do
    sp@SourcePack{..} <- askSalak
    case search ks source of
      Left  e     -> throwM $ SalakException (unpack ks) (toException $ PropException e)
      Right (k,t) -> do
        v <- runProp2 sp { source = t, pref = pref ++ unKeys k} fromProp
        case v of
          Left  e -> case fromException e of
            Just (SalakException a b) -> throwM $ SalakException a  b
            Just pe                   -> throwM $ SalakException (unpack ks) (toException pe)
            _                         -> throwM $ SalakException (unpack ks) e
          Right x -> return x

instance {-# OVERLAPPABLE #-} (m ~ t m', Monad m', Monad m, MonadTrans t, MonadSalak m') => MonadSalak m where
  askSalak = lift askSalak

instance Monad m => MonadSalak (Prop m) where
  askSalak = Prop ask

instance Monad m => MonadThrow (Prop m) where
  throwM = Prop . lift . throwError . toException

instance Monad m => MonadCatch (Prop m) where
  catch (Prop a) f = do
    sp <- ask
    v  <- lift $ runExceptT (runReaderT a sp)
    case v of
      Left  e -> case fromException e of
        Just ee -> f ee
        _       -> throwM e
      Right x -> return x

runProp2 :: Monad m => SourcePack -> Prop m a -> m (Either SomeException a)
runProp2 sp (Prop p) = runExceptT (runReaderT p sp)

runProp1 :: MonadThrow m => SourcePack -> Prop m a -> m a
runProp1 sp p = do
  v <- runProp2 sp p
  case v of
    Left  e -> throwM e
    Right x -> return x

runProp :: Monad m => SourcePack -> Prop m a -> Prop m a
runProp sp p = do
  v <- lift $ runProp2 sp p
  case v of
    Left  e -> throwM e
    Right x -> return x

instance FromProp m a => FromProp m (Maybe a) where
  fromProp = do
    v <- try fromProp
    case v of
      Left  e -> case fromException e of
        Just NullException         -> return Nothing
        Just (SalakException _ e2) -> case fromException e2 of
          Just NullException -> return Nothing
          _                  -> throwM e
        _                  -> throwM e
      Right a -> return (Just a)

instance FromProp m a => FromProp m (Either String a) where
  fromProp = do
    SourcePack{..} <- ask
    v <- try fromProp
    return $ case v of
      Left  e -> Left $ show (e :: SomeException)
      Right a -> Right a

instance {-# OVERLAPPABLE #-} FromProp m a => FromProp m [a] where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    foldM (go sp) [] $ sortBy g2 $ filter (isNum.fst) $ HM.toList $ TR.getMap source
    where
      go s vs (k,t) = (:vs) <$> runProp s { pref = pref s ++ [k], source = t} fromProp
      g2 (a,_) (b,_) = compare b a

instance {-# OVERLAPPABLE #-} (IsString s, FromProp m a) => FromProp m [(s, a)] where
  fromProp = do
    sp@SourcePack{..} <- askSalak
    foldM (go sp) [] $ sortBy g2 $ filter (isStr.fst) $ HM.toList $ TR.getMap source
    where
      go s vs (k,t) = (:vs) . (fromString $ show $ Keys [k],) <$> runProp s { pref = pref s ++ [k], source = t} fromProp
      g2 (a,_) (b,_) = compare b a

instance (Eq s, Hashable s, IsString s, FromProp m a) => FromProp m (HM.HashMap s a) where
  fromProp = HM.fromList <$> fromProp

instance (Eq s, Ord s, IsString s, FromProp m a) => FromProp m (M.Map s a) where
  fromProp = M.fromList <$> fromProp

-- | Supports for parsing `IO` value.
instance {-# OVERLAPPABLE #-} (MonadIO m, MonadIO n, FromProp (Either SomeException) a, FromProp m a) => FromProp m (n a) where
  fromProp = do
    sp <- ask
    a  <- fromProp
    lift $ liftIO <$> buildIO sp a

buildIO :: (MonadIO m, FromProp (Either SomeException) a) => SourcePack -> a -> m (IO a)
buildIO sp a = liftIO $ do
  aref <- newMVar a
  modifyMVar_ (qref sp) $ \f -> return $ \s ->
    let b = runProp1 sp {source = search2 s (pref sp), pref = pref sp} fromProp
    in case b of
      Left  e -> Left $ show e
      Right v -> do
        vb <- v
        io <- f s
        return (swapMVar aref (fromMaybe a vb) >> io)
  return (readMVar aref)

data SalakException
  = PropException String -- ^ Parse failed
  | NullException        -- ^ Not found
  | SalakException String SomeException
  deriving Show

instance Exception SalakException

-- | Automatic convert literal string into an instance of `Prop` @m@ @a@.
instance FromProp m a => IsString (Prop m a) where
  fromString ks = do
    sp@SourcePack{..} <- askSalak
    case search ks source of
      Left  e     -> throwM $ SalakException
        (if null pref then ks else show (Keys pref) ++ "." ++ ks)
        (toException $ PropException e)
      Right (k,t) -> runProp sp { source = t, pref = pref ++ unKeys k} fromProp

notFound :: Monad m => Prop m a
notFound = do
  SourcePack{..} <- askSalak
  throwM $ SalakException (show (Keys pref)) $ toException NullException

err :: Monad m => String -> Prop m a
err e = do
  SourcePack{..} <- askSalak
  throwM $ PropException $ show (Keys pref) ++ ":" ++ e

-- | Prop operators.
--
-- Suppose we have the following definition:
--
-- > data Config = Config
-- >   { enabled :: Bool
-- >   , level   :: IO LogLevel
-- >   }
class PropOp f a where
  -- | Parse or default value
  --
  -- > instance MonadThrow m => FromProp m Config where
  -- >   fromProp = Config
  -- >     <$> "enabled" .?= True
  -- >     <*> "level"   .?= (return LevelInfo)
  --
  -- IO value will work right.
  infixl 5 .?=
  (.?=) :: f a -> a -> f a
  -- | Parse or auto extract default value from a `Default` value
  --
  -- > instance Default Config where
  -- >   def = Config True (return LevelInfo)
  -- > instance MonadThrow m => FromProp m Config where
  -- >   fromProp = Config
  -- >     <$> "enabled" .?: enabled
  -- >     <$> "level"   .?: level
  infixl 5 .?:
  (.?:) :: Default b => f a -> (b -> a) -> f a
  (.?:) fa b = fa .?= b def

-- | Support for setting default normal value.
instance {-# OVERLAPPABLE #-} A.Alternative f => PropOp f a where
  (.?=) a b = a A.<|> pure b

-- | Support for setting default `IO` value.
instance (MonadIO m, FromProp (Either SomeException) a) => PropOp (Prop m) (IO a) where
  (.?=) ma a = do
    sp <- askSalak
    v  <- try ma
    case v of
      Left  (_ :: SomeException) -> liftIO a >>= buildIO sp
      Right o                    -> return o

instance Monad m => HasValid (Prop m) where
  invalid = err . toI18n

-- | Parse primitive value from `Value`
readPrimitive :: Monad m => (Value -> Either String a) -> Prop m a
readPrimitive f = do
  SourcePack{..} <- askSalak
  vx <- g $ TR.getPrimitive source >>= getVal
  case f <$> vx of
    Just (Left e)  -> err e
    Just (Right a) -> return a
    _              -> notFound
  where
    g = return

-- | Parse enum value from `Text`
readEnum :: Monad m => (Text -> Either String a) -> Prop m a
readEnum = readPrimitive . go
  where
    go f (VT t) = f t
    go _ x      = Left $ fst (typeOfV x) ++ " cannot convert to enum"


class Monad m => GFromProp m f where
  gFromProp :: Prop m (f a)

instance {-# OVERLAPPABLE #-} (Monad m, Constructor c, GFromProp m a) => GFromProp m (M1 C c a) where
    gFromProp
      | conIsRecord m = fmap M1 gFromProp
      | otherwise     = fmap M1 $ gEnum $ T.pack (conName m)
      where m = undefined :: t c a x

gEnum :: (Monad m, GFromProp m f) => Text -> Prop m (f a)
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

instance {-# OVERLAPPABLE #-} (Monad m, GFromProp m a, GFromProp m b) => GFromProp m (a:+:b) where
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

instance Monad m => FromProp m Bool where
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

instance Monad m => FromProp m Text where
  fromProp = readPrimitive go
    where
      go (VT x) = Right x
      go x      = Right $ T.pack $ snd $ typeOfV x

instance Monad m => FromProp m TL.Text where
  fromProp = TL.fromStrict <$> fromProp

instance Monad m => FromProp m B.ByteString where
  fromProp = TB.encodeUtf8 <$> fromProp

instance Monad m => FromProp m BL.ByteString where
  fromProp = TBL.encodeUtf8 <$> fromProp

instance Monad m => FromProp m String where
  fromProp = T.unpack <$> fromProp

instance Monad m => FromProp m Scientific where
  fromProp = readPrimitive go
    where
      go (VT x) = case readMaybe $ T.unpack x of
        Just v -> Right v
        _      -> Left  "string convert number failed"
      go (VI x) = Right x
      go x      = Left $ getType x ++ " cannot be number"

instance Monad m => FromProp m Float where
  fromProp = toRealFloat <$> fromProp

instance Monad m => FromProp m Double where
  fromProp = toRealFloat <$> fromProp

instance Monad m => FromProp m Integer where
  fromProp = toInteger <$> (fromProp :: Prop m Int)

instance Monad m => FromProp m Int where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Int8 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Int16 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Int32 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Int64 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Word where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Word8 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Word16 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Word32 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m Word64 where
  fromProp = fromProp >>= toNum

instance Monad m => FromProp m NominalDiffTime where
  fromProp = fromInteger <$> fromProp

instance Monad m => FromProp m DiffTime where
  fromProp = fromInteger <$> fromProp

instance (HasResolution a, Monad m) => FromProp m (Fixed a) where
  fromProp = fromInteger <$> fromProp

toNum :: (Monad m, Integral i, Bounded i) => Scientific -> Prop m i
toNum s = case toBoundedInteger s of
  Just v -> return v
  _      -> err "scientific number doesn't fit in the target representation"

instance Monad m => FromProp m CBool where
  fromProp = do
    b <- fromProp
    return $ if b then 1 else 0

instance Monad m => FromProp m CShort where
  fromProp = CShort <$> fromProp

instance Monad m => FromProp m CUShort where
  fromProp = CUShort <$> fromProp

instance Monad m => FromProp m CInt where
  fromProp = CInt <$> fromProp

instance Monad m => FromProp m CUInt where
  fromProp = CUInt <$> fromProp

instance Monad m => FromProp m CLong where
  fromProp = CLong <$> fromProp

instance Monad m => FromProp m CULong where
  fromProp = CULong <$> fromProp

instance Monad m => FromProp m CLLong where
  fromProp = CLLong <$> fromProp

instance Monad m => FromProp m CULLong where
  fromProp = CULLong <$> fromProp

instance Monad m => FromProp m CFloat where
  fromProp = CFloat <$> fromProp

instance Monad m => FromProp m CDouble where
  fromProp = CDouble <$> fromProp

