{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Either
import           Data.List       (intercalate)
import           Data.Text       (Text, pack, unpack)
import           GHC.Generics
import           Salak.Prop
import           Salak.Types
import           Test.Hspec
import           Test.QuickCheck


main = hspec spec

spec :: Spec
spec = do
  describe "Salak.Types" specProperty

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
      selectors "xx"    `shouldBe` Right [STxt "xx"]
      selectors "xx[0]" `shouldBe` Right [STxt "xx", SNum 0]
      selectors "xx.yy" `shouldBe` Right [STxt "xx", STxt "yy"]
    it "QuickCheck" $ do
      quickCheck $ \s -> let s' = unKey s in (toKey . reverse <$> selectors s') `shouldBe` Right (unpack s')
  context "source" $ do
    it "normal" $ do
      nullSource emptySource `shouldBe` True
      sizeSouce  emptySource `shouldBe` 0
    it "normal - 2" $ do
      let s1 = insert "hello" (VStr 0 "world") emptySource
      sizeSouce s1           `shouldBe` 1
      let s2 = insert "1"     (VStr 0 "world") emptySource
      sizeSouce s2           `shouldBe` 1
      let (es,_) = extractErr s2
      length es              `shouldBe` 0
    it "merge" $ do
      let s  = insert "hello" (VStr 0 "world") emptySource
          s1 = insert "hello" (VStr 0 "xxxxx") emptySource
          (e2, s2) = extractErr $ replace 0 emptySource s
      s2 `shouldBe` emptySource
      e2 `shouldBe` ["Del hello"]
      let (e3, s3) = extractErr $ replace 0 s1 s
      e3 `shouldBe` ["Mod hello"]
      s3 `shouldBe` s1
      let (e4, s4) = extractErr $ replace 0 s emptySource
      e4 `shouldBe` ["Add hello"]
      s4 `shouldBe` s
      let (e5, s5) = extractErr $ replace 0 s s
      e5 `shouldBe` []
      s5 `shouldBe` s
  context "Generic" $ do
    it "conf" $ do
      (e, sp) <- runSourcePackT $ loadMock
        [ ("name", "Daniel")
        , ("age", "18")
        , ("male", "yes")
        -- , ("det.hello", "world")
        ]
      e `shouldBe` []
      let a = search "" sp :: Either String Conf
      print a
      isRight a `shouldBe` True








