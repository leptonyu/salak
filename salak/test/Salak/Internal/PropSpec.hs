module Salak.Internal.PropSpec where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Data.Int
import           Salak.Internal.Prop
import           Salak.Internal.Source
import           Salak.Internal.Val
import qualified Salak.Trie              as T
import           Test.Hspec
-- import           Test.QuickCheck

newSource :: Value -> Source
newSource v = T.singleton $ modVals (Val 0 v) emptyVals

newSourcePack :: Value -> IO SourcePack
newSourcePack v = do
  let source = newSource v
      pref   = mempty
      reload = return $ ReloadResult False []
  qref <- newMVar $ \_ -> Right (return ())
  lref <- newMVar $ \_ -> return ()
  return SourcePack{..}

spec :: SpecWith ()
spec = do
  context "VT" $ do
    it "text" $ do
      sp <- newSourcePack (VT "128")
      let run :: forall m a. ReaderT SourcePack m a -> m a
          run = (`runReaderT` sp)
      vInt   :: Int   <- run $ require ""
      vInt32 :: Int32 <- run $ require ""
      -- vInt8  :: Int8  <- run $ require ""
      128 `shouldBe` vInt
      128 `shouldBe` vInt32
      -- print vInt8

