module Salak.Internal.SourceSpec(spec) where

import           Control.Monad.Writer
import           Data.Text             (Text, unpack)
import           GHC.Generics
import           Salak
import           Salak.Internal
import           Salak.Internal.Source
import qualified Salak.Trie            as T
import           System.Random         (randomIO)
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
      let (_,b,c) = extract T.empty $ gen 0 ([("hello", "world")] :: [(Text, Text)])
      length c `shouldBe` 0
      length (T.toList b) `shouldBe` 1
    it "normal - 2" $ do
      let (_,b,c) = extract T.empty $ gen 0 ([("hello.value", "world")] :: [(Text, Text)])
      length c `shouldBe` 0
      length (T.toList b) `shouldBe` 1
  context "Reload test" $ do
    it "reload" $ do
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
