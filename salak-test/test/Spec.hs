{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Either
import           Data.List            (intercalate)
import           Data.Text            (Text, pack, unpack)
import           GHC.Generics
import           Salak
import           Salak.Load.Toml
import           Salak.Load.Yaml
import           Test.Hspec
import           Test.QuickCheck

main = hspec spec

spec :: Spec
spec = do
  describe "Salak" extProperty

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

exts = loaders $ YAML :|: TOML

extProperty = do
  context "multiload" $ do
    it "salak" $ do
      loadAndRunSalak (loadByExt exts "test/salak") $ do
        cf <- require "me.icymint.conf"
        sp <- ask
        lift $ do
          print sp
          print cf
          name cf `shouldBe` "daniel"
          age  cf `shouldBe` 18
          male cf `shouldBe` True
          det  cf `shouldBe` SubConf "abc"
