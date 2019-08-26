module Salak.Internal.PropSpec(spec) where

import           Control.Monad.Writer
import           Data.Either
import           Data.Int
import           Data.Scientific
import           Data.Text            (Text)
import           Data.Word
import           Salak
import           Salak.Internal
import           Test.Hspec

data Conf = Conf
  { val :: Int
  }

instance FromProp m Conf where
  fromProp = Conf <$> "val" .?= 1

spec :: SpecWith ()
spec = do
  context "Prop" $ do
    let vals =
          [ ("int",  "128")
          , ("bool-1", "yes")
          , ("bool-2", "no")
          , ("bool-3", "true")
          , ("bool-4", "false")
          , ("bool-L1", "YES")
          , ("bool-L2", "NO")
          , ("bool-L3", "TRUE")
          , ("bool-L4", "FALSE")
          , ("int-", "-1")
          , ("empty", "")
          , ("a.val", "")
          , ("b.val", "wrong")
          ]
        run :: ((forall a. FromProp IO a => Text -> IO a) -> RunSalak ()) -> IO ()
        run g = loadAndRunSalak (loadMock vals) $ do
          sp <- askSourcePack
          let f k = case toKeys k of
                Left  e -> fail e
                Right x -> runProp sp (withKeys x fromProp)
          g f
    it "bool" $ run $ \r -> do
      b1  <- require "bool-1"
      b2  <- require "bool-2"
      b3  <- require "bool-3"
      b4  <- require "bool-4"
      bL1 <- require "bool-L1"
      bL2 <- require "bool-L2"
      bL3 <- require "bool-L3"
      bL4 <- require "bool-L4"
      lift $ do
        b1  `shouldBe` True
        b2  `shouldBe` False
        b3  `shouldBe` True
        b4  `shouldBe` False
        bL1 `shouldBe` True
        bL2 `shouldBe` False
        bL3 `shouldBe` True
        bL4 `shouldBe` False
        (r "int" :: IO Bool) `shouldThrow` anyException
    it "scientific" $ run $ \_ -> do
      n   <- require "int"
      n_  <- require "int-"
      lift $ do
        (n  :: Scientific) `shouldBe` 128
        (n_ :: Scientific) `shouldBe` (-1)
    it "int" $ run $ \r -> do
      v   <- require "int"
      v16 <- require "int"
      v32 <- require "int"
      v64 <- require "int"
      vm  <- require "empty"
      lift $ do
        (v    :: Int)     `shouldBe` 128
        (v16  :: Int16)   `shouldBe` 128
        (v32  :: Int32)   `shouldBe` 128
        (v64  :: Int64)   `shouldBe` 128
        (vm :: Maybe Int) `shouldBe` Nothing
        (r "int" :: IO Int8) `shouldThrow` anyException
    it "word" $ run $ \_ -> do
      v   <- require "int"
      v8  <- require "int"
      v16 <- require "int"
      v32 <- require "int"
      v64 <- require "int"
      lift $ do
        (v    :: Word)   `shouldBe` 128
        (v8   :: Word8)  `shouldBe` 128
        (v16  :: Word16) `shouldBe` 128
        (v32  :: Word32) `shouldBe` 128
        (v64  :: Word64) `shouldBe` 128
    it "maybe" $ run $ \_ -> do
      v  <- require "int"
      n  <- require "int.not.found"
      ev <- require "int"
      en <- require "int.not.found"
      lift $ do
        (v :: Maybe Int)          `shouldBe` Just 128
        (n :: Maybe Int)          `shouldBe` Nothing
        (ev :: Either String Int) `shouldBe` Right 128
        (en :: Either String Int) `shouldSatisfy` isLeft
    it "text" $ run $ \_ -> do
      v  <- require "empty"
      vm <- require "empty"
      n  <- require "not.found"
      lift $ do
        (v  :: String)       `shouldBe` ""
        (vm :: Maybe String) `shouldBe` Just ""
        (n  :: Maybe String) `shouldBe` Nothing
    it "<|>" $ run $ \r -> do
      Conf{..} <- require "a"
      lift $ do
        val `shouldBe` 1
        (r "b" :: IO Conf) `shouldThrow` anyException
  context "placeholder" $ do
    it "positive" $ do
      mkValue (VT "${}")             `shouldBe` Right (VR [VRR "" []])
      mkValue (VT "${:${}}")         `shouldBe` Right (VR [VRR "" [VRR "" []]])
      mkValue (VT "${a.b}")          `shouldBe` Right (VR [VRR "a.b" []])
      mkValue (VT "${xxxx}")         `shouldBe` Right (VR [VRR "xxxx" []])
      mkValue (VT "${xxxx:}")        `shouldBe` Right (VR [VRR "xxxx" [VRT ""]])
      mkValue (VT "${xxxx:7}")       `shouldBe` Right (VR [VRR "xxxx" [VRT "7"]])
      mkValue (VT "${xxxx:\\}}")     `shouldBe` Right (VR [VRR "xxxx" [VRT "}"]])
      mkValue (VT "${xxxx:${yyy}}")  `shouldBe` Right (VR [VRR "xxxx" [VRR "yyy" []]])
      mkValue (VT "--${xxxx:7}")     `shouldBe` Right (VR [VRT "--",  VRR "xxxx" [VRT "7"]])
      mkValue (VT "${xxxx:7}\\}")    `shouldBe` Right (VR [VRR "xxxx" [VRT "7"], VRT "}"])
      mkValue (VT "128")             `shouldBe` Right (VT "128")
      mkValue (VT "\\$")             `shouldBe` Right (VT "$")
      mkValue (VT "\\\\")            `shouldBe` Right (VT "\\")
      mkValue (VT "\\{")             `shouldBe` Right (VT "{")
      mkValue (VT "\\}")             `shouldBe` Right (VT "}")
    it "negative" $ do
      mkValue (VT "${${}}")          `shouldSatisfy` isLeft
      mkValue (VT "${128")           `shouldSatisfy` isLeft
      mkValue (VT "$")               `shouldSatisfy` isLeft
      mkValue (VT "{}")              `shouldSatisfy` isLeft
      mkValue (VT "}")               `shouldSatisfy` isLeft
    it "parse placeholder" $ do
      let vals =
            [ ("hello","${hey}")
            , ("hey","girl")
            , ("ok", "${xx:girl}")
            , ("nk", "${xx}")
            , ("world", "${hey}, world")
            , ("a", "${b}")
            , ("b", "${a}")
            , ("c", "${:${}}")
            , ("d", "${:}")
            ]
      loadAndRunSalak (loadMock vals) $ do
        v <- require "hello"
        x <- require "hey"
        t <- require "world"
        y <- require "a"
        z <- require "ok"
        w <- require "nk"
        c <- require "c"
        d <- require "d"
        lift $ do
          (x :: String) `shouldBe` "girl"
          (v :: String) `shouldBe` "girl"
          (z :: String) `shouldBe` "girl"
          (t :: String) `shouldBe` "girl, world"
          (d :: String) `shouldBe` ""
          (w :: Either String String) `shouldSatisfy` isLeft
          (y :: Either String String) `shouldSatisfy` isLeft
          (c :: Either String String) `shouldSatisfy` isLeft


