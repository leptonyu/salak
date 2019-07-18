{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoOverloadedLists          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
module Salak.Internal(
    load
  , loadTrie
  , UpdateResult(..)
  , runTrie
  , runSource
  , receive
  , receive_
  , fetchTrie
  , searchTrie
  , RunSalak
  , RunSalakT
  , HasSalak(..)
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
  , ToKeys(..)
  , setVal
  , Val(..)
  , Value(..)
  , ToValue(..)
  ) where


import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Control.Monad.IO.Unlift as IU
import qualified Control.Monad.Reader    as MR
import qualified Control.Monad.State     as MS
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import           Data.Text               (Text, pack)
import qualified Data.Text               as TT
import           Salak.Internal.Key
import           Salak.Internal.Prop
import           Salak.Internal.Trie
import           Salak.Internal.Val
import qualified Salak.Trie              as T
import           System.Directory
import           System.Environment

data UpdateTrie = UpdateTrie
  {  ref    :: MVar Source
  ,  refNo  :: Int
  ,  refMap :: HashMap Int String
  ,  update :: IO ( TraceSource -- Updated Tries
                  , IO ())    -- Confirm action
  }

loadTrie :: Bool -> String -> (Int -> IO TraceSource) -> RunSalak ()
loadTrie canReload name f = RunSalakT $ do
  UpdateTrie{..} <- MS.get
  v              <- liftIO $ readMVar ref
  ts             <- liftIO $ loadSource f refNo (fmap ([],) v)
  let (t,_,es) = extract v ts
  if null es
    then do
      let nut = UpdateTrie ref (refNo + 1) (HM.insert refNo name refMap) (go update refNo)
      _ <- liftIO $ swapMVar ref t
      MS.put nut
    else error $ show es
  where
    go ud n = if canReload
      then do
        (c,d) <- ud
        c1    <- loadSource f n c
        return (c1,d)
      else ud

load :: (Foldable f, ToKeys k, ToValue v) => Bool -> String -> IO (f (k,v)) -> RunSalak ()
load canReload name iof = loadTrie canReload name go
  where
    go i = generate i <$> iof

data UpdateResult = UpdateResult
  { hasErr :: Bool
  , msgs   :: [String]
  } deriving Show

runT :: MonadIO m => RunSalakT m a -> m (a, UpdateTrie)
runT (RunSalakT m) = do
  r <- liftIO $ newMVar T.empty
  MS.runStateT m (UpdateTrie r 0 HM.empty $ return (T.empty,return ()))

packT :: UpdateTrie -> (IO Source, IO UpdateResult)
packT UpdateTrie{..} = (readMVar ref, go)
  where
    go = do
      (t,a) <- update
      t0    <- readMVar ref
      let (t1, cs, es) = extract t0 t
      if null es
        then a >> swapMVar ref t1 >> return (UpdateResult False $ lines $ show cs)
        else return (UpdateResult True es)

-- | Execute all loading functions and return reloadable action
runTrie :: MonadIO m => RunSalakT m (IO UpdateResult -> IO a) -> m a
runTrie m = do
  (fa, ut) <- runT m
  liftIO $ fa $ snd (packT ut)

-- | return Source from `RunSalakT`
trie :: MonadIO m => RunSalakT m Source
trie = RunSalakT $ do
  UpdateTrie{..} <- MS.get
  liftIO $ readMVar ref

-- | Test function for return a `Source`
runSource :: RunSalak () -> IO Source
runSource ts = runTrie (ts >> receive trie)

receive :: RunSalak a -> RunSalak (IO UpdateResult -> IO a)
receive ra = ra >>= \a -> return (\_ -> return a)

receive_ :: RunSalak (IO UpdateResult -> IO ())
receive_ = return (\_ -> return ())

fetchTrie :: (ToKeys k, FromProp a, MonadIO m) => k -> RunSalakT m (Either String (IO a))
fetchTrie = fetchTrie1 True

searchTrie :: (ToKeys k, FromProp a, HasSalak m) => k -> m (Either String a)
searchTrie k = parse k <$> askSalak

fetchTrie1 :: (ToKeys k, FromProp a, MonadIO m) => Bool -> k -> RunSalakT m (Either String (IO a))
fetchTrie1 canReload k = RunSalakT $ do
    UpdateTrie{..} <- MS.get
    t              <- liftIO $ readMVar ref
    case parse k t of
      Left  e -> return (Left e)
      Right v -> if not canReload then return (Right $ return v) else do
        rv <- liftIO $ newMVar v
        MS.put $ UpdateTrie ref refNo refMap $ do
          (t1@(T.Trie w m), ac) <- update
          case parse k t of
            Left e1 -> return (T.Trie (add e1 w) m, ac)
            Right x -> return (t1, swapMVar rv x >> ac)
        return (Right $ readMVar rv)
    where
      add e Nothing       = Just ([e], emptyVals)
      add e (Just (es,v)) = Just (e:es, v)

parse :: forall k a. (ToKeys k, FromProp a) => k -> Source -> Either String a
parse k t = case search k t of
  Left  e            -> Left e
  Right (Keys k1,t1) -> case runProp (k1, t1) (fromProp :: Prop (Either String a)) of
    F _ e -> Left e
    N _   -> Left "null"
    O _ v -> v

newtype RunSalakT  m a = RunSalakT  { unRun  :: MS.StateT UpdateTrie m a } deriving (Functor, Applicative, Monad, MS.MonadTrans)

type RunSalak = RunSalakT IO

instance MonadIO m => MonadIO (RunSalakT m) where
  liftIO = RunSalakT . liftIO

instance IU.MonadUnliftIO m => IU.MonadUnliftIO (RunSalakT m) where
  askUnliftIO = RunSalakT $ do
    ut <- MS.get
    f  <- MS.lift IU.askUnliftIO
    return $ IU.UnliftIO $ IU.unliftIO f . (`MS.evalStateT` ut) . unRun

class Monad m => HasSalak m where
  askSalak :: m Source

instance MonadIO m => HasSalak (RunSalakT m) where
  askSalak = trie

instance Monad m => HasSalak (MR.ReaderT Source m) where
  askSalak = MR.ask

-- | Load mock variables into `Source`
loadMock :: [(Text, Text)] -> RunSalak ()
loadMock fa = load False "mock" (return fa)

-- | Load environment variables into `Source`
loadEnv :: RunSalak ()
loadEnv = load False "environment" go
  where
    go = concat . fmap split2 . filter ((/= '_') . head . fst) <$> getEnvironment
    split2 (k,v) = [(TT.pack k,v),(convert k,v)]
    convert = TT.toLower . TT.pack . map (\c -> if c == '_' then '.' else c)

-- | Convert arguments to properties
type ParseCommandLine = [String] -> IO [(Text, Text)]

-- | Default way to parse command line arguments
defaultParseCommandLine :: ParseCommandLine
defaultParseCommandLine = return . mapMaybe go
  where
    go ('-':'-':as) = case break (=='=') as of
      (a,'=':b) -> Just (pack a, pack b)
      _         -> Nothing
    go _ = Nothing

-- | Default way to parse command line arguments
loadCommandLine :: ParseCommandLine -> RunSalak ()
loadCommandLine pcl = load False "commandLine" (getArgs >>= pcl)

-- | Try load file, if file does not exist then do nothing.
tryLoadFile :: MonadIO m => (FilePath -> RunSalakT m ()) -> FilePath -> RunSalakT m ()
tryLoadFile f file = do
  b <- liftIO $ doesFileExist file
  when b $ do
    liftIO $ putStrLn $ "Load " ++ file
    f file

