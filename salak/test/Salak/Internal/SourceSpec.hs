module Salak.Internal.SourceSpec(spec) where

import           Control.Concurrent.MVar
import           Control.Monad.Writer
import           Data.Text               (Text, unpack)
import           GHC.Generics
import           Salak
import           Salak.Internal
import qualified Salak.Trie              as T
import           System.Random           (randomIO)
import           Test.Hspec
import           Test.QuickCheck


data Config = Config
  { level :: IO Int
  , world :: Maybe Bool
  }

instance MonadIO m => FromProp m Config where
  fromProp = Config <$> "level" .?= (return 1) <*> "world"

data Hello = Hello
  { hello :: IO Int
  } deriving Generic

instance MonadIO m => FromProp m Hello

loadRandom :: (MonadThrow m, MonadIO m) => Text -> LoadSalakT m ()
loadRandom key = loadList True (unpack key) go
  where
    go :: IO [(Text, Int)]
    go = do
      a :: Int <- randomIO
      return [(key, a)]

spec :: SpecWith ()
spec = do
  context "source" $ do
    it "normal - 1" $ do
      let (_,b,c) = extract T.empty $ genSource 0 ([("hello", "world")] :: [(Text, Text)])
      length c `shouldBe` 0
      length (T.toList b) `shouldBe` 1
    it "normal - 2" $ do
      let (_,b,c) = extract T.empty $ genSource 0 ([("hello.value", "world")] :: [(Text, Text)])
      length c `shouldBe` 0
      length (T.toList b) `shouldBe` 1
  context "Reload test" $ do
    it "reload - random" $ do
      loadAndRunSalak (loadRandom "hello") $ do
        Hello{..}  <- require ""
        Config{..} <- require ""
        x  <- liftIO hello
        liftIO $ print x
        liftIO $ print world
        q1 <- liftIO level
        r  <- askReload
        lift $ quickCheck $ \(_ :: Int) -> do
          ReloadResult{..} <- liftIO r
          when hasError $ print msgs
          hasError `shouldBe` False
          msgs     `shouldBe` ["hello:Mod"]
          y  <- hello
          x  `shouldNotBe` y
          q2 <- level
          q1 `shouldBe` q2
    it "reload - error" $ do
      io <- newMVar [("hello", "10")]
      loadAndRunSalak (loadList True "test" (readMVar io :: IO [(Text, Text)])) $ do
        vio <- require "hello"
        rl  <- askReload
        lift $ do
          v <- vio :: IO Int
          v `shouldBe` 10
        lift $ do
          void $ swapMVar io [("hello", "text")]
          ReloadResult{..} <- rl
          hasError `shouldBe` True
          v <- vio
          v `shouldBe` 10
        lift $ do
          void $ swapMVar io [("hello", "5")]
          ReloadResult{..} <- rl
          hasError `shouldBe` False
          msgs `shouldBe` ["hello:Mod"]
          v <- vio
          v `shouldBe` 5
        lift $ do
          void $ swapMVar io []
          ReloadResult{..} <- rl
          hasError `shouldBe` False
          msgs `shouldBe` ["hello:Del"]
          v <- vio
          v `shouldBe` 10
        lift $ do
          void $ swapMVar io [("hello", "8")]
          ReloadResult{..} <- rl
          hasError `shouldBe` False
          msgs `shouldBe` ["hello:Add"]
          v <- vio
          v `shouldBe` 8
    it "reload - multi layer" $ do
      io0 <- newMVar [("hello", "10")]
      io1 <- newMVar [("hello", "8")]
      let
        lio = do
          loadList True "test-0" (readMVar io0 :: IO [(Text, Text)])
          loadList True "test-1" (readMVar io1 :: IO [(Text, Text)])
      loadAndRunSalak lio $ do
        vio <- require "hello"
        rl  <- askReload
        lift $ do
          v <- vio :: IO Int
          v `shouldBe` 10
        lift $ do
          void $ swapMVar io0 [("hello", "7")]
          _ <- rl
          v <- vio :: IO Int
          v `shouldBe` 7
        lift $ do
          void $ swapMVar io1 [("hello", "81")]
          _ <- rl
          v <- vio :: IO Int
          v `shouldBe` 7
        lift $ do
          void $ swapMVar io0 []
          _ <- rl
          v <- vio :: IO Int
          v `shouldBe` 81
        lift $ do
          void $ swapMVar io1 [("hello", "82")]
          _ <- rl
          v <- vio :: IO Int
          v `shouldBe` 82
        lift $ do
          void $ swapMVar io0 [("hello", "10")]
          _ <- rl
          v <- vio :: IO Int
          v `shouldBe` 10


