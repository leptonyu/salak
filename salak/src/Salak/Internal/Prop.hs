{-# LANGUAGE TypeFamilies               #-}
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
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TB
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TBL
import           Data.Time
import           Data.Word
import           Foreign.C
import           GHC.Exts
import           GHC.Generics
import           GHC.Stack
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

  {-# INLINE setLogF #-}
  setLogF :: MonadIO m => (CallStack -> Text -> IO ()) -> m ()
  setLogF f = do
    SourcePack{..} <- askSourcePack
    liftIO $ void $ swapMVar lref f

  {-# INLINE logSalak #-}
  logSalak :: (HasCallStack, MonadIO m) => Text -> m ()
  logSalak msg = do
    SourcePack{..} <- askSourcePack
    liftIO $ readMVar lref >>= \lf -> lf callStack msg

  -- | Parse properties using `FromProp`. For example:
  --
  -- > a :: Bool              <- require "bool.key"
  -- > b :: Maybe Int         <- require "int.optional.key"
  -- > c :: Either String Int <- require "int.error.key"
  -- > d :: IO Int            <- require "int.reloadable.key"
  --
  -- `require` supports parse `IO` values, which actually wrap a 'MVar' variable and can be reseted by reloading configurations.
  -- Normal value will not be affected by reloading configurations.
  require :: (MonadThrow m, MonadIO m, FromProp m a) => Text -> m a
  require ks = do
    s@SourcePack{..} <- askSourcePack
    runProp s $ case toKeys ks of
      Left  e -> failKey ks e
      Right k -> withKeys k fromProp

instance Monad m => MonadSalak (ReaderT SourcePack m) where
  {-# INLINE askSourcePack #-}
  askSourcePack = ask

-- | Property parser, used to parse property from `Value`
newtype Prop m a
  = Prop { unProp :: ReaderT SourcePack (ExceptT SomeException m) a }
  deriving (Functor, Applicative, Monad, MonadReader SourcePack, MonadIO)

-- | Automatic convert literal string into an instance of `Prop` @m@ @a@.
instance (MonadIO m, FromProp m a) => IsString (Prop m a) where
  {-# INLINE fromString #-}
  fromString ks = case toKeys ks of
    Left  e -> failKey (fromString ks) e
    Right k -> withKeys k fromProp

instance MonadTrans Prop where
  {-# INLINE lift #-}
  lift = Prop . lift . lift

instance Monad m => A.Alternative (Prop m) where
  {-# INLINE empty #-}
  empty = do
    SourcePack{..} <- ask
    throwM $ NullException pref

  {-# INLINE (<|>) #-}
  a <|> b = do
    v <- try a
    case v of
      Right x                   -> return x
      Left (_ :: SomeException) -> b

instance Monad m => MonadError SomeException (Prop m) where
  {-# INLINE throwError #-}
  throwError = Prop . lift . throwError . toException
  {-# INLINE catchError #-}
  catchError (Prop ma) me = Prop $ do
    c <- ask
    lift $ catchError (runReaderT ma c) (\e -> runReaderT (unProp $ me e) c)

instance Monad m => MonadThrow (Prop m) where
  {-# INLINE throwM #-}
  throwM = throwError . toException

instance Monad m => MonadCatch (Prop m) where
  {-# INLINE catch #-}
  catch ma me = catchError ma (\e -> maybe (throwM e) me $ fromException e)

instance Monad m => MonadFail (Prop m) where
  {-# INLINE fail #-}
  fail = failKey ""


{-# INLINE runProp #-}
runProp :: MonadThrow m => SourcePack -> Prop m a -> m a
runProp sp (Prop p) = do
  v <- runExceptT (runReaderT p sp)
  case v of
    Left  e -> throwM e
    Right x -> return x

{-# INLINE withProp #-}
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

-- | Exception
data SalakException
  = SalakException Keys String -- ^ Parse failed
  | NullException Keys        -- ^ Not found
  deriving Show

instance Exception SalakException

{-# INLINE failKey #-}
failKey :: Monad m => Text -> String -> Prop m a
failKey ks e = do
  SourcePack{..} <- ask
  throwM
    $ SalakException (pref <> singletonKey (KT ks)) e


-- | Type class used to parse properties.
class FromProp m a where

  -- | Parse properties from `Value`.
  fromProp :: MonadIO m => Prop m a
  default fromProp :: (Generic a, GFromProp m (Rep a), MonadIO m) => Prop m a
  {-# INLINE fromProp #-}
  fromProp = fmap to gFromProp

instance FromProp m a => FromProp m (Maybe a) where
  {-# INLINE fromProp #-}
  fromProp = do
    v <- try fromProp
    case v of
      Left  e -> case fromException e of
        Just (NullException _) -> return Nothing
        _                      -> throwM e
      Right a -> return (Just a)

instance FromProp m a => FromProp m (Either String a) where
  {-# INLINE fromProp #-}
  fromProp = do
    v <- try fromProp
    return $ case v of
      Left  e -> Left $ show (e :: SomeException)
      Right a -> Right a

instance {-# OVERLAPPABLE #-} FromProp m a => FromProp m [a] where
  {-# INLINE fromProp #-}
  fromProp = do
    SourcePack{..} <- ask
    sequence $ (`withKey` fromProp) <$> sort (filter isNum $ HM.keys $ TR.tmap source)

instance {-# OVERLAPPABLE #-} (IsString s, FromProp m a) => FromProp m [(s, a)] where
  {-# INLINE fromProp #-}
  fromProp = do
    SourcePack{..} <- ask
    sequence $ go <$> sort (filter isStr $ HM.keys $ TR.tmap source)
    where
      go k = (fromString $ show $ singletonKey k,) <$> withKey k fromProp

instance (Eq s, Hashable s, IsString s, FromProp m a) => FromProp m (HM.HashMap s a) where
  {-# INLINE fromProp #-}
  fromProp = HM.fromList <$> fromProp

instance (Eq s, Ord s, IsString s, FromProp m a) => FromProp m (M.Map s a) where
  {-# INLINE fromProp #-}
  fromProp = M.fromList <$> fromProp

-- | Supports for parsing `IO` value.
instance {-# OVERLAPPABLE #-} (MonadIO m, FromProp IO a, FromProp m a) => FromProp m (IO a) where
  {-# INLINE fromProp #-}
  fromProp = do
    sp <- ask
    a  <- fromProp
    buildIO sp a

{-# INLINE buildIO #-}
buildIO :: (MonadIO m, FromProp IO a) => SourcePack -> a -> m (IO a)
buildIO sp a = liftIO $ do
  aref <- newMVar a
  modifyMVar_ (qref sp) $ \f -> return $ \s -> do
    v <- runProp sp {source = s} $ withKeys (pref sp) fromProp
    io <- f s
    return $ swapMVar aref (fromMaybe a v) >> io
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
  {-# INLINE (.?:) #-}
  infixl 5 .?:
  (.?:) :: Default b => f a -> (b -> a) -> f a
  (.?:) fa b = fa .?= b def

-- | Support for setting default normal value.
instance {-# OVERLAPPABLE #-} A.Alternative f => PropOp f a where
  {-# INLINE (.?=) #-}
  (.?=) a b = a A.<|> pure b

-- | Support for setting default `IO` value.
instance (MonadIO m, FromProp IO a) => PropOp (Prop m) (IO a) where
  {-# INLINE (.?=) #-}
  (.?=) ma a = do
    sp <- ask
    v  <- try ma
    case v of
      Left  (_ :: SomeException) -> liftIO a >>= buildIO sp
      Right o                    -> return o

instance Monad m => HasValid (Prop m) where
  {-# INLINE invalid #-}
  invalid = Control.Monad.Fail.fail . toI18n

{-# INLINE readPrimitive' #-}
readPrimitive' :: (HasCallStack, MonadIO m) => (Value -> Either String a) -> Prop m (Maybe a)
readPrimitive' f = do
  SourcePack{..} <- ask
  liftIO $ readMVar lref >>= \lf -> lf callStack ("require: " <> showKey pref)
  let {-# INLINE go #-}
      go [VRT t]        = return t
      go (VRT t   : as) = (t <>) <$> go as
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
          go (VRT w : as)
      go [] = A.empty
      {-# INLINE g2 #-}
      g2 (VR r) = VT <$> go r
      g2 v      = return v
      {-# INLINE g3 #-}
      g3 vy
        | nullValue vy = return Nothing
        | otherwise    = case f vy of
            Left  e -> Control.Monad.Fail.fail e
            Right a -> return (Just a)
  maybe A.empty (g2 >=> g3) $ TR.tvar source >>= getVal

-- | Parse primitive value from `Value`
{-# INLINE readPrimitive #-}
readPrimitive :: MonadIO m => (Value -> Either String a) -> Prop m a
readPrimitive f = readPrimitive' f >>= maybe A.empty return

-- | Parse enum value from `Text`
{-# INLINE readEnum #-}
readEnum :: MonadIO m => (Text -> Either String a) -> Prop m a
readEnum = readPrimitive . go
  where
    {-# INLINE go #-}
    go f (VT t) = f t
    go _ x      = Left $ fst (typeOfV x) ++ " cannot convert to enum"


class GFromProp m f where
  gFromProp :: MonadIO m => Prop m (f a)

instance {-# OVERLAPPABLE #-} (Constructor c, GFromProp m a) => GFromProp m (M1 C c a) where
  {-# INLINE gFromProp #-}
  gFromProp
    | conIsRecord m = fmap M1 gFromProp
    | otherwise     = fmap M1 $ gEnum $ T.pack (conName m)
    where m = undefined :: t c a x

{-# INLINE gEnum #-}
gEnum :: (GFromProp m f, MonadIO m) => Text -> Prop m (f a)
gEnum va = do
  o <- gFromProp
  readEnum $ \x -> if x==va then Right o else Left "enum invalid"

instance {-# OVERLAPPABLE #-} (Selector s, GFromProp m a) => GFromProp m(M1 S s a) where
  {-# INLINE gFromProp #-}
  gFromProp = withKey (KT $ T.pack $ selName (undefined :: t s a p)) $ M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} GFromProp m a => GFromProp m (M1 D i a) where
  {-# INLINE gFromProp #-}
  gFromProp = M1 <$> gFromProp

instance {-# OVERLAPPABLE #-} FromProp m a => GFromProp m (K1 i a) where
  {-# INLINE gFromProp #-}
  gFromProp = fmap K1 fromProp

instance Monad m => GFromProp m U1 where
  {-# INLINE gFromProp #-}
  gFromProp = pure U1

instance {-# OVERLAPPABLE #-} (GFromProp m a, GFromProp m b) => GFromProp m (a:*:b) where
  {-# INLINE gFromProp #-}
  gFromProp = (:*:) <$> gFromProp <*> gFromProp

instance {-# OVERLAPPABLE #-} (GFromProp m a, GFromProp m b) => GFromProp m (a:+:b) where
  {-# INLINE gFromProp #-}
  gFromProp = fmap L1 gFromProp A.<|> fmap R1 gFromProp

instance FromProp m a => FromProp m (Identity a) where
  {-# INLINE fromProp #-}
  fromProp = Identity <$> fromProp

instance (FromProp m a, FromProp m b) => FromProp m (a,b) where
  {-# INLINE fromProp #-}
  fromProp = (,) <$> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c) => FromProp m(a,b,c) where
  {-# INLINE fromProp #-}
  fromProp = (,,) <$> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d) => FromProp m(a,b,c,d) where
  {-# INLINE fromProp #-}
  fromProp = (,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e) => FromProp m(a,b,c,d,e) where
  {-# INLINE fromProp #-}
  fromProp = (,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f) => FromProp m(a,b,c,d,e,f) where
  {-# INLINE fromProp #-}
  fromProp = (,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g) => FromProp m(a,b,c,d,e,f,g) where
  {-# INLINE fromProp #-}
  fromProp = (,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g, FromProp m h) => FromProp m(a,b,c,d,e,f,g,h) where
  {-# INLINE fromProp #-}
  fromProp = (,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp

instance (FromProp m a, FromProp m b, FromProp m c, FromProp m d, FromProp m e, FromProp m f, FromProp m g, FromProp m h, FromProp m i) => FromProp m(a,b,c,d,e,f,g,h,i) where
  {-# INLINE fromProp #-}
  fromProp = (,,,,,,,,) <$> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp <*> fromProp


instance FromProp m a => FromProp m (Min a) where
  {-# INLINE fromProp #-}
  fromProp = Min <$> fromProp

instance FromProp m a => FromProp m (Max a) where
  {-# INLINE fromProp #-}
  fromProp = Max <$> fromProp

instance FromProp m a => FromProp m (First a) where
  {-# INLINE fromProp #-}
  fromProp = First <$> fromProp

instance FromProp m a => FromProp m (Last a) where
  {-# INLINE fromProp #-}
  fromProp = Last <$> fromProp

instance FromProp m a => FromProp m (Dual a) where
  {-# INLINE fromProp #-}
  fromProp = Dual <$> fromProp

instance FromProp m a => FromProp m (Sum a) where
  {-# INLINE fromProp #-}
  fromProp = Sum <$> fromProp

instance FromProp m a => FromProp m (Product a) where
  {-# INLINE fromProp #-}
  fromProp = Product <$> fromProp

instance FromProp m a => FromProp m (Option a) where
  {-# INLINE fromProp #-}
  fromProp = Option <$> fromProp

instance FromProp m Bool where
  {-# INLINE fromProp #-}
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
  {-# INLINE fromProp #-}
  fromProp = fromMaybe "" <$> readPrimitive' go
    where
      {-# INLINE go #-}
      go (VT x) = Right x
      go x      = Right $ T.pack $ snd $ typeOfV x

instance FromProp m TL.Text where
  {-# INLINE fromProp #-}
  fromProp = TL.fromStrict <$> fromProp

instance FromProp m B.ByteString where
  {-# INLINE fromProp #-}
  fromProp = TB.encodeUtf8 <$> fromProp

instance FromProp m BL.ByteString where
  {-# INLINE fromProp #-}
  fromProp = TBL.encodeUtf8 <$> fromProp

instance FromProp m String where
  {-# INLINE fromProp #-}
  fromProp = T.unpack <$> fromProp

instance FromProp m Scientific where
  {-# INLINE fromProp #-}
  fromProp = readPrimitive go
    where
      {-# INLINE go #-}
      go (VT x) = case readMaybe $ T.unpack x of
        Just v -> Right v
        _      -> Left  "string convert number failed"
      go (VI x) = Right x
      go x      = Left $ getType x ++ " cannot be number"

instance FromProp m Float where
  {-# INLINE fromProp #-}
  fromProp = toRealFloat <$> fromProp

instance FromProp m Double where
  {-# INLINE fromProp #-}
  fromProp = toRealFloat <$> fromProp

instance FromProp m Integer where
  {-# INLINE fromProp #-}
  fromProp = toInteger <$> (fromProp :: Prop m Int)

instance FromProp m Int where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Int8 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Int16 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Int32 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Int64 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Word where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Word8 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Word16 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Word32 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m Word64 where
  {-# INLINE fromProp #-}
  fromProp = fromProp >>= toNum

instance FromProp m NominalDiffTime where
  {-# INLINE fromProp #-}
  fromProp = fromInteger <$> fromProp

instance FromProp m DiffTime where
  {-# INLINE fromProp #-}
  fromProp = fromInteger <$> fromProp

instance (HasResolution a, Monad m) => FromProp m (Fixed a) where
  {-# INLINE fromProp #-}
  fromProp = fromInteger <$> fromProp

{-# INLINE toNum #-}
toNum :: (Monad m, Integral i, Bounded i) => Scientific -> Prop m i
toNum s = case toBoundedInteger s of
  Just v -> return v
  _      -> Control.Monad.Fail.fail "scientific number doesn't fit in the target representation"

instance FromProp m CBool where
  {-# INLINE fromProp #-}
  fromProp = do
    b <- fromProp
    return $ if b then 1 else 0

instance FromProp m CShort where
  {-# INLINE fromProp #-}
  fromProp = CShort <$> fromProp

instance FromProp m CUShort where
  {-# INLINE fromProp #-}
  fromProp = CUShort <$> fromProp

instance FromProp m CInt where
  {-# INLINE fromProp #-}
  fromProp = CInt <$> fromProp

instance FromProp m CUInt where
  {-# INLINE fromProp #-}
  fromProp = CUInt <$> fromProp

instance FromProp m CLong where
  {-# INLINE fromProp #-}
  fromProp = CLong <$> fromProp

instance FromProp m CULong where
  {-# INLINE fromProp #-}
  fromProp = CULong <$> fromProp

instance FromProp m CLLong where
  {-# INLINE fromProp #-}
  fromProp = CLLong <$> fromProp

instance FromProp m CULLong where
  {-# INLINE fromProp #-}
  fromProp = CULLong <$> fromProp

instance FromProp m CFloat where
  {-# INLINE fromProp #-}
  fromProp = CFloat <$> fromProp

instance FromProp m CDouble where
  {-# INLINE fromProp #-}
  fromProp = CDouble <$> fromProp

