{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Salak.Internal
-- Copyright:   2019 Daniel YU
-- License:     MIT
-- Maintainer:  leptonyu@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- This module is used for implementing loaders.
--
module Salak.Internal(
    loadAndRunSalak'
  , loadAndRunSalak
  , loadTrie
  , loadList
  , LoadSalakT
  , LoadSalak
  , RunSalakT
  , RunSalak
  , MonadSalak(..)
  , loadMock
  , loadEnv
  , loadCommandLine
  , ParseCommandLine
  , defaultParseCommandLine
  , tryLoadFile
  , Source
  , TraceSource
  , Keys(..)
  , Key(..)
  , simpleKeys
  , fromKeys
  , ToKeys(..)
  , setVal
  , Val(..)
  , Value(..)
  , ToValue(..)
  , liftNT
  , SourcePack(..)
  , MonadIO
  , module Salak.Internal.Writable
  ) where


import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Control.Monad.IO.Unlift as IU
import           Control.Monad.Reader
import qualified Control.Monad.State     as MS
import           Data.Char               (toLower)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import qualified Data.Set                as S
import           Data.String
import           Data.Text               (Text, pack)
import qualified Data.Text               as TT
import           GHC.Stack
import           Salak.Internal.Key
import           Salak.Internal.Prop
import           Salak.Internal.Source
import           Salak.Internal.Val
import           Salak.Internal.Writable
import qualified Salak.Trie              as T
import           System.Directory
import           System.Environment

data UpdateSource = UpdateSource
  {  ref    :: !(MVar Source)
  ,  refNo  :: !Int
  ,  refMap :: !(HashMap Int String)
  ,  lfunc  :: !(MVar LFunc)
  ,  qfunc  :: !(MVar QFunc)
  ,  update :: !(MVar (IO ( TraceSource -- Updated Tries
                  , IO ())))    -- Confirm action
  }

-- | Configuration Loader Monad, used for load properties from sources. Custom loaders using `loadTrie`
newtype LoadSalakT m a = LoadSalakT (MS.StateT UpdateSource m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MS.MonadState UpdateSource, MonadThrow, MonadCatch)

-- | Simple IO Monad
type LoadSalak = LoadSalakT IO

{-# INLINE runLoad #-}
runLoad :: Monad m => LoadSalakT m a -> UpdateSource -> m a
runLoad (LoadSalakT ma) = MS.evalStateT ma

{-# INLINE liftNT #-}
liftNT :: MonadIO m => LoadSalak () -> LoadSalakT m ()
liftNT a = MS.get >>= liftIO . runLoad a

instance MonadIO m => MonadSalak (LoadSalakT m) where
  askSourcePack = MS.get >>= toSourcePack
  setLogF f = do
    UpdateSource{..} <- MS.get
    liftIO $ void $ swapMVar lfunc f
  logSalak msg = do
    UpdateSource{..} <- MS.get
    liftIO $ readMVar lfunc >>= \lf -> lf callStack msg

instance (MonadThrow m, IU.MonadUnliftIO m) => IU.MonadUnliftIO (LoadSalakT m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do
    ut <- MS.get
    lift $ IU.withUnliftIO $ \u -> return (IU.UnliftIO (IU.unliftIO u . flip runLoad ut))

-- | Standard `MonadSalak` instance.
newtype RunSalakT m a = RunSalakT { runSalakT :: ReaderT SourcePack m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader SourcePack, MonadThrow, MonadCatch)

-- | Simple IO Monad
type RunSalak = RunSalakT IO

instance MonadIO m => MonadSalak (RunSalakT m) where
  {-# INLINE askSourcePack #-}
  askSourcePack = ask

instance (MonadThrow m, IU.MonadUnliftIO m) => IU.MonadUnliftIO (RunSalakT m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = do
    ut <- ask
    lift $ IU.withUnliftIO $ \u -> return (IU.UnliftIO (IU.unliftIO u . flip runReaderT ut . runSalakT))

-- | Basic loader
loadTrie :: (MonadThrow m, MonadIO m) => Bool -> String -> (Int -> IO TraceSource) -> LoadSalakT m ()
loadTrie !canReload !name f = do
  logSalak $ "Loading " <> (if canReload then "[reloadable]" else "") <> fromString name
  UpdateSource{..} <- MS.get
  (MS.put=<<) $ liftIO $ do
    v  <- readMVar ref
    ts <- loadSource f refNo (fmap ([],) v)
    let (t,_,es) = extract v ts
    if null es
      then do
        modifyMVar_ update $ go ts refNo
        _ <- swapMVar ref t
        return $ UpdateSource{ refNo = refNo + 1, refMap = HM.insert refNo name refMap, .. }
      else fail $ unlines es
  where
    {-# INLINE go #-}
    go ts n ud = return $ do
      (c,d) <- ud
      c1    <- loadSource (if canReload then f else (\_ -> return ts)) n c
      return (c1,d)

-- | Simple loader
loadList :: (MonadThrow m, MonadIO m, Foldable f, ToKeys k, ToValue v) => Bool -> String -> IO (f (k,v)) -> LoadSalakT m ()
loadList canReload name iof = loadTrie canReload name (\i -> gen i <$> iof)

-- | Standard salak functions, by load and with a `SourcePack` instance.
--  Users should use `SourcePack` to create custom `MonadSalak` instances, then you get will an instance of `MonadSalak`.
loadAndRunSalak' :: (MonadThrow m, MonadIO m) => LoadSalakT m () -> (SourcePack -> m a) -> m a
loadAndRunSalak' lstm f = load lstm >>= f

-- | Standard salak functions, by load and run with `RunSalakT`.
loadAndRunSalak :: (MonadThrow m, MonadIO m) => LoadSalakT m () -> RunSalakT m a -> m a
loadAndRunSalak lstm = loadAndRunSalak' lstm . runReaderT . runSalakT

load :: (MonadThrow m, MonadIO m) => LoadSalakT m () -> m SourcePack
load lm = do
  us <- liftIO $ do
    r <- newMVar T.empty
    q <- newMVar $ return . void . swapMVar r
    u <- newMVar $ return (T.empty, return ())
    l <- newMVar $ \_ _ -> return ()
    return $ UpdateSource r 0 HM.empty l q u
  runLoad (lm >> MS.get) us >>= toSourcePack

{-# INLINE toSourcePack #-}
toSourcePack :: MonadIO m => UpdateSource -> m SourcePack
toSourcePack UpdateSource{..} = liftIO $ do
  s <- readMVar ref
  return
    $ SourcePack s s S.empty mempty qfunc lfunc
    $ do
      t        <- readMVar ref
      (ts, ac) <- join $ readMVar update
      let (t1,cs,es) = extract t ts
      f <- readMVar qfunc
      if null es
        then flip catch (\e -> return . ReloadResult True . lines . show $ (e :: SomeException)) $ do
          a <- f t1
          ac >> a >> return (ReloadResult False $ lines $ show cs)
        else return (ReloadResult True es)

-- | Load mock variables into `Source`
loadMock :: (MonadThrow m, MonadIO m) => [(Text, Text)] -> LoadSalakT m ()
loadMock fa = loadList False "mock" (return fa)

-- | Load environment variables into `Source`
loadEnv :: (MonadThrow m, MonadIO m) => LoadSalakT m ()
loadEnv = loadList False "environment" go
  where
    {-# INLINE go #-}
    go = fmap split2 . filter ((/= '_') . head . fst) <$> getEnvironment
    {-# INLINE split2 #-}
    split2 (k,v) = (convert k, mkValue' $ TT.pack v)
    {-# INLINE mkValue' #-}
    mkValue' v = case mkValue (VT v) of
      Left  _ -> VR [VRT v]
      Right x -> x
    {-# INLINE convert #-}
    convert = TT.pack . map (\c -> if c == '_' then '.' else toLower c)

-- | Convert arguments to properties
type ParseCommandLine = [String] -> IO [(Text, Text)]

-- | Default way to parse command line arguments
defaultParseCommandLine :: ParseCommandLine
defaultParseCommandLine = return . mapMaybe go
  where
    {-# INLINE go #-}
    go ('-':'-':as) = case break (=='=') as of
      (a,'=':b) -> Just (pack a, pack b)
      _         -> Nothing
    go _ = Nothing

-- | Default way to parse command line arguments
loadCommandLine :: (MonadThrow m, MonadIO m) => ParseCommandLine -> LoadSalakT m ()
loadCommandLine pcl = loadList False "commandLine" (getArgs >>= pcl)

-- | Try load file, if file does not exist then do nothing.
{-# INLINE tryLoadFile #-}
tryLoadFile :: MonadIO m => (FilePath -> LoadSalakT m ()) -> FilePath -> LoadSalakT m ()
tryLoadFile f file = do
  b <- liftIO $ doesFileExist file
  if b
    then f file
    else logSalak $ "File does not exist, ignore load " <> fromString file

