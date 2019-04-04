{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Writer
import           Data.Either
import           Data.List            (intercalate)
import           Data.Menshen
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
  fromProp = SubConf <$> "hello" .?= "yyy" ? pattern "[a-z]{3,16}"

instance FromProp Conf

specProperty = do
  context "selectors" $ do
    it "normal" $ do
      selectors ""         `shouldBe` Right []
      selectors "."        `shouldBe` Right []
      selectors ".."       `shouldBe` Right []
      selectors "xx"       `shouldBe` Right [SStr "xx"]
      selectors "xx[0]"    `shouldBe` Right [SStr "xx", SNum 0]
      selectors "xx.yy"    `shouldBe` Right [SStr "xx", SStr "yy"]
      selectors "xx[0][1]" `shouldBe` Right [SStr "xx", SNum 0, SNum 1]
      toKey (reverse $ SStr "x" : (SNum <$> [0..9])) `shouldBe` "x[0][1][2][3][4][5][6][7][8][9]"
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
      length e2           `shouldBe` 0
    it "normal - 4" $ do
      let (s3,e3) = runWriter $ insert "a.b"   (VStr 0 "world") emptySource
      print s3
      sizeSource s3       `shouldBe` 1
      length e3           `shouldBe` 0
  context "source - merge" $ do
    let s  = fst $ runWriter $ insert "hello" (VStr 0 "world") emptySource
        so = fst $ runWriter $ insert "hello" (VStr 1 "yyyyy") emptySource
        s1 = fst $ runWriter $ insert "hello" (VStr 0 "xxxxx") emptySource
    it "merge - del" $ do
      let (s2,e2) = runWriter $ replace 0 emptySource s
      s2 `shouldBe` emptySource
      e2 `shouldBe` ["#0 Del hello"]
      let (s3,e3) = runWriter $ replace 1 emptySource s
      s3 `shouldBe` s
      e3 `shouldBe` []
    it "merge - mod" $ do
      let (s3,e3) = runWriter $ replace 0 s1 s
      e3 `shouldBe` ["#0 Mod hello"]
      s3 `shouldBe` s1
      let (s2,e2) = runWriter $ replace 1 s1 s
      e2 `shouldBe` []
      s2 `shouldBe` s
      let (_,e4) = runWriter $ replace 1 so s
      e4 `shouldBe` ["#1 Add hello"]
    it "merge - add" $ do
      let (s4,e4) = runWriter $ replace 0 s emptySource
      e4 `shouldBe` ["#0 Add hello"]
      s4 `shouldBe` s
      let (s2,e2) = runWriter $ replace 1 s emptySource
      s2 `shouldBe` emptySource
      e2 `shouldBe` []
      let (s3,e3) = runWriter $ replace 1 so emptySource
      e3 `shouldBe` ["#1 Add hello"]
      s3 `shouldBe` so
    it "merge - unchange" $ do
      let (s5,e5) = runWriter $ replace 0 s s
      e5 `shouldBe` []
      s5 `shouldBe` s
  context "Generic" $ do
    it "conf" $ do
      sp <- runLoadT Nothing $ loadMock
        [ ("name", "Daniel")
        , ("age", "18")
        , ("male", "yes")
        ]
      errs sp `shouldBe` []
      let a = search "" sp :: Either String Conf
      print a
      isRight a `shouldBe` True








