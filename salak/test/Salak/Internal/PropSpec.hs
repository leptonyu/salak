module Salak.Internal.PropSpec where

import           Control.Concurrent.MVar
import           Control.Monad.Writer
import           Data.Either
import           Data.Int
import qualified Data.Set                as S
import           Salak
import           Salak.Internal
import           Salak.Internal.Val
import qualified Salak.Trie              as T
import           Test.Hspec

newSource :: Value -> Source
newSource v = case modVals (Val 0 v) emptyVals of
  Left  _ -> T.empty
  Right x -> T.singleton x

newSourcePack :: Value -> IO SourcePack
newSourcePack v = do
  let source = newSource v
      origin = source
      kref   = S.empty
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
      let run :: forall m a. Monad m => RunSalakT m a -> m a
          run = (`runRun` sp)
      vInt   <- run $ require ""
      vInt32 <- run $ require ""
      128 `shouldBe` (vInt   :: Int)
      128 `shouldBe` (vInt32 :: Int32)
  context "placeholder" $ do
    it "placeholder" $ do
      mkValue (VT "${xxxx}")         `shouldBe` Right (VR [VRR "xxxx" []])
      mkValue (VT "${xxxx:}")        `shouldBe` Right (VR [VRR "xxxx" [VRT ""]])
      mkValue (VT "${xxxx:7}")       `shouldBe` Right (VR [VRR "xxxx" [VRT "7"]])
      mkValue (VT "${xxxx:\\}}")     `shouldBe` Right (VR [VRR "xxxx" [VRT "}"]])
      mkValue (VT "${xxxx:${yyy}}")  `shouldBe` Right (VR [VRR "xxxx" [VRR "yyy" []]])
      mkValue (VT "--${xxxx:7}")     `shouldBe` Right (VR [VRT "--",  VRR "xxxx" [VRT "7"]])
      mkValue (VT "${xxxx:7}\\}")    `shouldBe` Right (VR [VRR "xxxx" [VRT "7"], VRT "}"])
      mkValue (VT "128")             `shouldBe` Right (VT "128")
      mkValue (VT "${128")           `shouldSatisfy` isLeft
    it "source" $ do
      let vals =
            [ ("hello","${hey}")
            , ("hey","girl")
            , ("ok", "${xx:girl}")
            , ("nk", "${xx}")
            , ("a", "${b}")
            , ("b", "${a}")
            ]
      loadAndRunSalak (loadMock vals) $ do
        v <- require "hello"
        x <- require "hey"
        y <- require "a"
        z <- require "ok"
        w <- require "nk"
        lift $ do
          (x :: String) `shouldBe` "girl"
          (v :: String) `shouldBe` "girl"
          (z :: String) `shouldBe` "girl"
          (w :: Either String String) `shouldSatisfy` isLeft
          (y :: Either String String) `shouldSatisfy` isLeft


