{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Either
import           Data.List            (intercalate)
import           Data.Text            (Text, pack, unpack)
import           GHC.Generics
import           Salak
import           Salak.Prop
import           Salak.Types
import           Salak.Types.Selector
import           Salak.Types.Source
import           Test.Hspec
import           Test.QuickCheck

main = hspec spec

spec :: Spec
spec = do
  describe "Salak.Types" specProperty
  describe "Salak.Json"  jsonProperty
  describe "Salak.Toml"  tomlProperty
  describe "Salak"       extProperty

newtype SKey = SKey { unKey :: Text } deriving Show

instance Arbitrary SKey where
  arbitrary = do
    key <- choose (1,20)
    vs  <- vectorOf key $ do
      k <- choose (1,20)
      v <- vectorOf k $ choose ('a','z')
      b <- choose (0,10) :: Gen Int
      if b > 0 then return v else do
        x <- choose (0,10) :: Gen Int
        return (v ++ "[" ++ show x ++ "]")
    return (SKey $ pack $ intercalate "." vs)

data Conf = Conf
  { name :: String
  , age  :: Int
  , male :: Bool
  , det  :: SubConf
  } deriving (Eq, Show, Generic)

data SubConf = SubConf
  { hello :: String } deriving (Eq, Show, Generic)

instance FromProp SubConf where
  fromProp = SubConf <$> "hello" .?= "yyy"

instance FromProp Conf

specProperty = do
  context "selectors" $ do
    it "normal" $ do
      selectors ""      `shouldBe` Right []
      selectors "."     `shouldBe` Right []
      selectors ".."    `shouldBe` Right []
      selectors "xx"    `shouldBe` Right [SStr "xx"]
      selectors "xx[0]" `shouldBe` Right [SStr "xx", SNum 0]
      selectors "xx.yy" `shouldBe` Right [SStr "xx", SStr "yy"]
    it "QuickCheck" $ do
      quickCheck $ \s -> let s' = unKey s in (toKey . reverse <$> selectors s') `shouldBe` Right (unpack s')
  context "source" $ do
    it "normal" $ do
      sizeSource emptySource `shouldBe` 0
    it "normal - 2" $ do
      let (s1,e1) = runWriter $ insert "hello" (VStr 0 "world") emptySource
      sizeSource s1       `shouldBe` 1
      length e1           `shouldBe` 0
    it "normal - 3" $ do
      let (s2,e2) = runWriter $ insert "1"     (VStr 0 "world") emptySource
      sizeSource s2       `shouldBe` 1
      length e2           `shouldBe` 00
    it "normal - 4" $ do
      let (s3,e3) = runWriter $ insert "a.b"   (VStr 0 "world") emptySource
      print s3
      sizeSource s3       `shouldBe` 1
      length e3           `shouldBe` 0
  context "source - merge" $ do
    let s  = fst $ runWriter $ insert "hello" (VStr 0 "world") emptySource
        s1 = fst $ runWriter $ insert "hello" (VStr 0 "xxxxx") emptySource
    it "merge - del" $ do
      let (s2,e2) = runWriter $ replace 0 emptySource s
      s2 `shouldBe` emptySource
      e2 `shouldBe` ["#0 Del hello"]
    it "merge - mod" $ do
      let (s3,e3) = runWriter $ replace 0 s1 s
      e3 `shouldBe` ["#0 Mod hello"]
      s3 `shouldBe` s1
    it "merge - add" $ do
      let (s4,e4) = runWriter $ replace 0 s emptySource
      e4 `shouldBe` ["#0 Add hello"]
      s4 `shouldBe` s
    it "merge - unchange" $ do
      let (s5,e5) = runWriter $ replace 0 s s
      e5 `shouldBe` []
      s5 `shouldBe` s
  context "Generic" $ do
    it "conf" $ do
      sp <- runSourcePackT $ loadMock
        [ ("name", "Daniel")
        , ("age", "18")
        , ("male", "yes")
        ]
      errs sp `shouldBe` []
      let a = search "" sp :: Either String Conf
      print a
      isRight a `shouldBe` True

jsonProperty = do
  context "load json" $ do
    it "salak.yml" $ do
      flip loadSalak (loadYaml "test/salak.yml") $ do
        as <- require "array"
        cf <- require "me.icymint.conf"
        lift $ do
          as      `shouldBe` ["a","b","d","c" :: String]
          name cf `shouldBe` "daniel"
          age  cf `shouldBe` 18
          male cf `shouldBe` True
          det  cf `shouldBe` SubConf "abc"

tomlProperty = do
  context "load toml" $ do
    it "salak.toml" $ do
      flip loadSalak (loadToml "test/salak.toml") $ do
        cf <- require "me.icymint.conf"
        lift $ do
          name cf `shouldBe` "shelly"
          age  cf `shouldBe` 16
          male cf `shouldBe` False
          det  cf `shouldBe` SubConf "def"

extProperty = do
  context "multiload" $ do
    it "salak" $ do
      flip loadSalak (defaultLoadByExt "test/salak") $ do
        cf <- require "me.icymint.conf"
        sp <- ask
        lift $ do
          print sp
          print cf
          name cf `shouldBe` "daniel"
          age  cf `shouldBe` 18
          male cf `shouldBe` True
          det  cf `shouldBe` SubConf "abc"










