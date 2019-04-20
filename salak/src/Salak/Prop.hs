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
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module Salak.Prop where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Default
import           Data.Int
import qualified Data.Map.Strict         as M
import           Data.Menshen
import           Data.Scientific
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TB
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TBL
import           Data.Time.Clock
import           Data.Word
import           GHC.Exts
import           GHC.Generics            hiding (Selector)
import qualified GHC.Generics            as G
import           Salak.Types
import           Salak.Types.Selector
import           Salak.Types.Source
import           Salak.Types.Value
import           Text.Read               (readMaybe)

data PResult a
  = O [Selector] a      -- ^ Succeed value
  | N [Selector]        -- ^ Empty value
  | F [Selector] String -- ^ Fail value
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

data PropSource = PropSource
  { originSP :: SourcePack
  , currSP   :: SourcePack
  , cacheRef :: M.Map [Selector] Bool
  }

newtype PropT m a = Prop { unProp :: ReaderT PropSource m a }
  deriving (Functor, Applicative, Monad, MonadTrans, Alternative)

-- | Optional value.
infixl 5 .?=
(.?=) :: Alternative f => f a -> a -> f a
(.?=) a b = a <|> pure b

-- | Default value.
infixl 5 .?:
(.?:) :: (Alternative f, Default b) => f a -> (b -> a) -> f a
(.?:) fa b = fa .?= b def

-- | Monad used to parse properties to destination type.
type Prop = PropT PResult

runProp sp a = runReaderT (unProp a) sp

askSub :: (SourcePack -> SourcePack) -> Prop PropSource
askSub f = do
  ps <- Prop ask
  return ps { currSP = f (currSP ps) }

askOrigin :: Prop SourcePack
askOrigin = originSP <$> Prop ask

instance MonadReader SourcePack Prop where
  ask = currSP <$> Prop ask
  local f (Prop a) = Prop (local (\sp -> sp { currSP = f (currSP sp) }) a)

instance HasValid Prop where
  invalid = err . toI18n

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
      go sp = select sp (SStr $ T.pack $ selName (undefined :: t s a p))

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
    fps <- askSub id
    lift $ case runProp fps (fromProp :: Prop a) of
      O s a -> O s $ Just a
      N s   -> O s Nothing
      F s e -> F s e

instance {-# OVERLAPPABLE #-} FromProp a => FromProp [a] where
  fromProp = do
    sp@SourcePack{..} <- ask
    as <- foldM (go sp) [] $ M.toList (mapValue source)
    return (reverse as)
    where
      go sp' as (ix,s) = do
        so <- askSub $ \_ -> sp' { prefix = ix : prefix sp', source = s}
        a <- lift $ runProp so fromProp
        return (a:as)

instance {-# OVERLAPPABLE #-} FromEnumProp a => FromProp a where
  fromProp = readPrimitive $ \ss v -> case v of
    VStr  _ s -> either (F ss) (O ss) $ fromEnumProp $ T.toLower s
    x         -> F ss $ getType x ++ " cannot be enum"

evalV :: [Selector] -> Value -> Prop Value
evalV x (VRef i rs) = do
  sp <- askOrigin
  ps <- askSub (\_ -> sp)
  if M.member x (cacheRef ps)
    then lift $ F x "self reference"
    else lift $ VStr i <$> foldM (go ps { cacheRef = M.insert x True $ cacheRef ps} ) "" rs
  where
    go _  a (RVal b) = return (T.append a b)
    go ps a (RRef f) = case convert $ runProp ps (selectP f) of
        Right b -> return (T.append a b)
        Left  e -> F f e
evalV _ v           = return v

-- | ReadPrimitive value
readPrimitive :: ([Selector] -> Value -> PResult a) -> Prop a
readPrimitive f = do
  SourcePack{..} <- ask
  case getQ (value source) of
    Just v -> evalV prefix v >>= lift . f prefix
    _      -> lift $ N prefix

class FromEnumProp a where
  fromEnumProp :: Text -> Either String a
  {-# MINIMAL fromEnumProp #-}

err :: String -> Prop a
err e = do
  sp <- ask
  lift $ F (prefix sp) e

-- | Parse value
readSelect :: FromProp a => Text -> Prop a
readSelect key = case selectors key of
  Left  e -> err e
  Right s -> selectP s

selectP :: FromProp a => [Selector] -> Prop a
selectP s = local (\sp -> foldl select sp s) fromProp

search :: FromProp a => Text -> SourcePack -> Either String a
search key sp = convert $ runProp (PropSource sp sp M.empty) (readSelect key)

convert :: PResult a -> Either String a
convert (O _ x) = Right x
convert (N s  ) = Left $ "key " ++ toKey s ++ " not found"
convert (F s e) = Left $ "key " ++ toKey s ++ " : " ++ e

instance FromProp Bool where
  fromProp = readPrimitive go
    where
      go s (VBool _ x) = O s x
      go s (VStr  _ x) = case T.toLower x of
        "true"  -> O s True
        "yes"   -> O s True
        "false" -> O s False
        "no"    -> O s False
        _       -> F s "string convert bool failed"
      go s x             = F s $ getType x ++ " cannot be bool"

instance FromProp Text where
  fromProp = readPrimitive go
    where
      go s (VStr  _ x) = O s x
      go s x           = O s $ T.pack (getV x)

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
      go s (VStr  _ x) = case readMaybe $ T.unpack x of
        Just v -> O s v
        _      -> F s "string convert number failed"
      go s (VNum  _ x) = O s x
      go s x           = F s $ getType x ++ " cannot be number"

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

instance FromProp NominalDiffTime where
  fromProp = fromInteger <$> fromProp

toNum :: (Integral i, Bounded i) => Scientific -> Prop i
toNum s = case toBoundedInteger s of
  Just v -> return v
  _      -> err "scientific number doesn't fit in the target representation"
