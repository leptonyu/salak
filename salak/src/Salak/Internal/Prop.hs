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

import           Control.Applicative     ((<|>))
import qualified Control.Applicative     as A
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Identity  (Identity (..))
import           Control.Monad.Reader
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Default
import           Data.Fixed
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HM
import           Data.Int
import           Data.List               (sort)
import qualified Data.Map.Strict         as M
import           Data.Maybe
import           Data.Menshen
import           Data.Scientific
import           Data.Semigroup
import qualified Data.Set                as S
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
import           Unsafe.Coerce           (unsafeCoerce)


-- | Core type class of salak, which provide function to parse properties.
class Monad m => MonadSalak m where

  -- | Monad has the ability to get a SourcePack instance.
  askSourcePack :: m SourcePack

  -- | Get reload action which used for reload profiles
  askReload :: m (IO ReloadResult)
  askReload = reload <$> askSourcePack

  setLogF :: MonadIO m => (String -> IO ()) -> m ()
  setLogF f = do
    SourcePack{..} <- askSourcePack
    liftIO $ void $ swapMVar lref f

  logSalak :: MonadIO m => String -> m ()
  logSalak msg = do
    SourcePack{..} <- askSourcePack
    liftIO $ readMVar lref >>= ($ msg)

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
    s <- askSourcePack
    runProp s $ case toKeys ks of
      Left  e -> failKey (unpack ks) (PropException e)
      Right k -> withKeys k fromProp

instance Monad m => MonadSalak (ReaderT SourcePack m) where
  askSourcePack = ask

-- | Property parser, used to parse property from `Value`
newtype Prop m a
  = Prop { unProp :: ReaderT SourcePack (ExceptT SomeException m) a }
  deriving (Functor, Applicative, Monad, MonadReader SourcePack, MonadIO)

runProp :: MonadThrow m => SourcePack -> Prop m a -> m a
runProp sp (Prop p) = do
  v <- runExceptT (runReaderT p sp)
  case v of
    Left  e -> throwM e
    Right x -> return x

withProp :: (SourcePack -> SourcePack) -> Prop m a -> Prop m a
withProp = unsafeCoerce withReaderT

{-# INLINE withKey #-}
withKey :: Key -> Prop m a -> Prop m a
withKey = withKeys . singletonKey

{-# INLINE withKeys #-}
withKeys :: Keys -> Prop m a -> Prop m a
withKeys key = withProp
  $ \SourcePack{..} ->
     SourcePack{pref = pref <> key, source = TR.subTries key source, ..}

data SalakException
  = PropException String -- ^ Parse failed
  | NullException        -- ^ Not found
  | SalakException String SomeException
  deriving Show

instance Exception SalakException


{-# INLINE failKey #-}
failKey :: Monad m => String -> SalakException -> Prop m a
failKey ks e = do
  SourcePack{..} <- ask
  throwM
    $ SalakException (go (show pref) ks)
    $ toException e
  where
    {-# INLINE go #-}
    go "" a = a
    go a "" = a
    go a  b = a <> "." <> b

-- | Automatic convert literal string into an instance of `Prop` @m@ @a@.
instance (Monad m, FromProp m a) => IsString (Prop m a) where
  fromString ks = case toKeys ks of
    Left  e -> failKey ks (PropException e)
    Right k -> withKeys k fromProp


instance MonadTrans Prop where
  lift = Prop . lift . lift

instance Monad m => A.Alternative (Prop m) where
  empty = failKey "" NullException

  a <|> b = do
    v <- try a
    case v of
      Right x                   -> return x
      Left (_ :: SomeException) -> b

instance Monad m => MonadError SomeException (Prop m) where
  throwError = Prop . lift . throwError . toException
  catchError (Prop ma) me = Prop $ do
    c <- ask
    lift $ catchError (runReaderT ma c) (\e -> runReaderT (unProp $ me e) c)

instance Monad m => MonadThrow (Prop m) where
  throwM = throwError . toException

instance Monad m => MonadCatch (Prop m) where
  catch ma me = catchError ma (\e -> maybe (throwM e) me $ fromException e)

instance Monad m => MonadFail (Prop m) where
  fail = failKey "" . PropException

-- | Type class used to parse properties.
class FromProp m a where

  -- | Parse properties from `Value`.
  fromProp :: Monad m => Prop m a
  default fromProp :: (Generic a, GFromProp m (Rep a), Monad m) => Prop m a
  fromProp = fmap to gFromProp

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
    v <- try fromProp
    return $ case v of
      Left  e -> Left $ show (e :: SomeException)
      Right a -> Right a

instance {-# OVERLAPPABLE #-} FromProp m a => FromProp m [a] where
  fromProp = do
    SourcePack{..} <- ask
    sequence $ (`withKey` fromProp) <$> sort (filter isNum $ HM.keys $ TR.getMap source)

instance {-# OVERLAPPABLE #-} (IsString s, FromProp m a) => FromProp m [(s, a)] where
  fromProp = do
    SourcePack{..} <- ask
    sequence $ go <$> sort (filter isStr $ HM.keys $ TR.getMap source)
    where
      go k = (fromString $ show $ singletonKey k,) <$> withKey k fromProp

instance (Eq s, Hashable s, IsString s, FromProp m a) => FromProp m (HM.HashMap s a) where
  fromProp = HM.fromList <$> fromProp

instance (Eq s, Ord s, IsString s, FromProp m a) => FromProp m (M.Map s a) where
  fromProp = M.fromList <$> fromProp

-- | Supports for parsing `IO` value.
instance {-# OVERLAPPABLE #-} (MonadIO m, FromProp (Either SomeException) a, FromProp m a) => FromProp m (IO a) where
  fromProp = do
    sp <- ask
    a  <- fromProp
    buildIO sp a

{-# INLINE buildIO #-}
buildIO :: (MonadIO m, FromProp (Either SomeException) a) => SourcePack -> a -> m (IO a)
buildIO sp a = liftIO $ do
  aref <- newMVar a
  modifyMVar_ (qref sp) $ \f -> return $ \s ->
    case runProp sp {source = s} $ withKeys (pref sp) fromProp of
      Left  e -> Left $ show e
      Right v -> do
        vb <- v
        io <- f s
        return (swapMVar aref (fromMaybe a vb) >> io)
  return (readMVar aref)


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
    sp <- ask
    v  <- try ma
    case v of
      Left  (_ :: SomeException) -> liftIO a >>= buildIO sp
      Right o                    -> return o

instance Monad m => HasValid (Prop m) where
  invalid = Control.Monad.Fail.fail . toI18n

{-# INLINE readPrimitive' #-}
readPrimitive' :: Monad m => (Value -> Either String a) -> Prop m (Maybe a)
readPrimitive' f = do
  SourcePack{..} <- ask
  let {-# INLINE go #-}
      go (VRT t   : as) = if null as then return t else (t <>) <$> go as
      go (VRR k d : as) = if S.member k kref
        then Control.Monad.Fail.fail $ "reference cycle of key " <> show k
        else do
          t <- withProp (\_ -> SourcePack
                { pref = k
                , source = TR.subTries k origin
                , kref = S.insert k kref
                , .. }) fromProp
          w <- case t of
            Just x -> return x
            _      -> if null d then A.empty else go d
          if null as then return w else (w <>) <$> go as
      go [] = A.empty
      {-# INLINE g2 #-}
      g2 (VR r) = VT <$> go r
      g2 v      = return v
  case TR.getPrimitive source >>= getVal of
    Just v -> do
      vy <- g2 v
      if nullValue vy
        then return Nothing
        else case f vy of
          Left  e -> Control.Monad.Fail.fail e
          Right a -> return (Just a)
    _      -> A.empty

-- | Parse primitive value from `Value`
{-# INLINE readPrimitive #-}
readPrimitive :: Monad m => (Value -> Either String a) -> Prop m a
readPrimitive f = readPrimitive' f >>= maybe A.empty return

-- | Parse enum value from `Text`
readEnum :: Monad m => (Text -> Either String a) -> Prop m a
readEnum = readPrimitive . go
  where
    {-# INLINE go #-}
    go f (VT t) = f t
    go _ x      = Left $ fst (typeOfV x) ++ " cannot convert to enum"


class GFromProp m f where
  gFromProp :: Monad m => Prop m (f a)

instance {-# OVERLAPPABLE #-} (Constructor c, GFromProp m a) => GFromProp m (M1 C c a) where
    gFromProp
      | conIsRecord m = fmap M1 gFromProp
      | otherwise     = fmap M1 $ gEnum $ T.pack (conName m)
      where m = undefined :: t c a x

gEnum :: (GFromProp m f, Monad m) => Text -> Prop m (f a)
gEnum va = do
  o <- gFromProp
  readEnum $ \x -> if x==va then Right o else Left "enum invalid"

instance {-# OVERLAPPABLE #-} (Selector s, GFromProp m a) => GFromProp m(M1 S s a) where
  gFromProp = withKey (KT $ T.pack $ selName (undefined :: t s a p)) $ M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} GFromProp m a => GFromProp m (M1 D i a) where
  gFromProp = M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} FromProp m a => GFromProp m (K1 i a) where
    gFromProp = fmap K1 fromProp

instance Monad m => GFromProp m U1 where
  gFromProp = pure U1

instance {-# OVERLAPPABLE #-} (GFromProp m a, GFromProp m b) => GFromProp m (a:*:b) where
  gFromProp = (:*:) <$> gFromProp <*> gFromProp

instance {-# OVERLAPPABLE #-} (GFromProp m a, GFromProp m b) => GFromProp m (a:+:b) where
  gFromProp = fmap L1 gFromProp A.<|> fmap R1 gFromProp

instance FromProp m a => FromProp m (Identity a) where
  fromProp = Identity <$> fromProp

instance (FromProp m a, FromProp m b) => FromProp m (a,b) where
  fromProp = (,) <$> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c) => FromProp m(a,b,c) where
  fromProp = (,,) <$> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d) => FromProp m(a,b,c,d) where
  fromProp = (,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e) => FromProp m(a,b,c,d,e) where
  fromProp = (,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f) => FromProp m(a,b,c,d,e,f) where
  fromProp = (,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g) => FromProp m(a,b,c,d,e,f,g) where
  fromProp = (,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g, FromProp m h) => FromProp m(a,b,c,d,e,f,g,h) where
  fromProp = (,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g, FromProp m h, FromProp m i) => FromProp m(a,b,c,d,e,f,g,h,i) where
  fromProp = (,,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp


instance FromProp m a => FromProp m (Min a) where
  fromProp = Min <$> fromProp

instance FromProp m a => FromProp m (Max a) where
  fromProp = Max <$> fromProp

instance FromProp m a => FromProp m (First a) where
  fromProp = First <$> fromProp

instance FromProp m a => FromProp m (Last a) where
  fromProp = Last <$> fromProp

instance FromProp m a => FromProp m (Dual a) where
  fromProp = Dual <$> fromProp

instance FromProp m a => FromProp m (Sum a) where
  fromProp = Sum <$> fromProp

instance FromProp m a => FromProp m (Product a) where
  fromProp = Product <$> fromProp

instance FromProp m a => FromProp m (Option a) where
  fromProp = Option <$> fromProp

instance FromProp m Bool where
  fromProp = readPrimitive go
    where
      {-# INLINE go #-}
      go (VB x) = Right x
      go (VT x) = case T.toLower x of
        "true"  -> Right True
        "yes"   -> Right True
        "false" -> Right False
        "no"    -> Right False
        _       -> Left "string convert bool failed"
      go x      = Left $ getType x ++ " cannot be bool"

instance FromProp m Text where
  fromProp = fromMaybe "" <$> readPrimitive' go
    where
      {-# INLINE go #-}
      go (VT x) = Right x
      go x      = Right $ T.pack $ snd $ typeOfV x

instance FromProp m TL.Text where
  fromProp = TL.fromStrict <$> fromProp

instance FromProp m B.ByteString where
  fromProp = TB.encodeUtf8 <$> fromProp

instance FromProp m BL.ByteString where
  fromProp = TBL.encodeUtf8 <$> fromProp

instance FromProp m String where
  fromProp = T.unpack <$> fromProp

instance FromProp m Scientific where
  fromProp = readPrimitive go
    where
      {-# INLINE go #-}
      go (VT x) = case readMaybe $ T.unpack x of
        Just v -> Right v
        _      -> Left  "string convert number failed"
      go (VI x) = Right x
      go x      = Left $ getType x ++ " cannot be number"

instance FromProp m Float where
  fromProp = toRealFloat <$> fromProp

instance FromProp m Double where
  fromProp = toRealFloat <$> fromProp

instance FromProp m Integer where
  fromProp = toInteger <$> (fromProp :: Prop m Int)

instance FromProp m Int where
  fromProp = fromProp >>= toNum

instance FromProp m Int8 where
  fromProp = fromProp >>= toNum

instance FromProp m Int16 where
  fromProp = fromProp >>= toNum

instance FromProp m Int32 where
  fromProp = fromProp >>= toNum

instance FromProp m Int64 where
  fromProp = fromProp >>= toNum

instance FromProp m Word where
  fromProp = fromProp >>= toNum

instance FromProp m Word8 where
  fromProp = fromProp >>= toNum

instance FromProp m Word16 where
  fromProp = fromProp >>= toNum

instance FromProp m Word32 where
  fromProp = fromProp >>= toNum

instance FromProp m Word64 where
  fromProp = fromProp >>= toNum

instance FromProp m NominalDiffTime where
  fromProp = fromInteger <$> fromProp

instance FromProp m DiffTime where
  fromProp = fromInteger <$> fromProp

instance (HasResolution a, Monad m) => FromProp m (Fixed a) where
  fromProp = fromInteger <$> fromProp

{-# INLINE toNum #-}
toNum :: (Monad m, Integral i, Bounded i) => Scientific -> Prop m i
toNum s = case toBoundedInteger s of
  Just v -> return v
  _      -> Control.Monad.Fail.fail "scientific number doesn't fit in the target representation"

instance FromProp m CBool where
  fromProp = do
    b <- fromProp
    return $ if b then 1 else 0

instance FromProp m CShort where
  fromProp = CShort <$> fromProp

instance FromProp m CUShort where
  fromProp = CUShort <$> fromProp

instance FromProp m CInt where
  fromProp = CInt <$> fromProp

instance FromProp m CUInt where
  fromProp = CUInt <$> fromProp

instance FromProp m CLong where
  fromProp = CLong <$> fromProp

instance FromProp m CULong where
  fromProp = CULong <$> fromProp

instance FromProp m CLLong where
  fromProp = CLLong <$> fromProp

instance FromProp m CULLong where
  fromProp = CULLong <$> fromProp

instance FromProp m CFloat where
  fromProp = CFloat <$> fromProp

instance FromProp m CDouble where
  fromProp = CDouble <$> fromProp

