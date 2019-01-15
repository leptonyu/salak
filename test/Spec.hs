{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.HashMap.Strict    as M
import           Data.Salak
import           Data.Salak.CommandLine
import           Data.Salak.Environment
import           Data.Salak.Operation   ()
import           Data.Text              (Text, intercalate, pack)
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck

main = hspec spec

spec :: Spec
spec = do
  describe "Data.Salak.Types" specProperty
  describe "Data.Salak"       specProperties

shouldFail :: (Show a, Eq a) => a -> a -> Expectation
shouldFail f a = (f `shouldBe` a) `shouldThrow` anyErrorCall

data Config = Config
  { name :: Text
  , dir  :: Text
  , ext  :: Int
  } deriving (Eq, Show)

instance FromProperties Config where
  fromProperties v = Config
        <$> v .?> "name"
        <*> v .?> "dir"
        <*> v .?> "ext" .?= 1

specProperty = do
  context "empty" $ do
    it "normal" $ do
      let p = empty
      print p
      p `shouldBe` Properties [] []
  context "toKeys" $ do
    it "normal" $ do
      []        `shouldBe` toKeys ""
      ["a"]     `shouldBe` toKeys "a"
      ["a"]     `shouldBe` toKeys "a."
      ["a","b"] `shouldBe` toKeys "a.b"
      ["a","b"] `shouldBe` toKeys "a.....b"
    it "quickCheck" $ do
      quickCheck $ \a -> let b = toKeys $ pack a in b == toKeys (intercalate "." b)
  context "insert" $ do
    let p = PStr "Hello"
        k = toKeys "a.b"
        m = insert k p empty
    it "normal" $ do
      insert [] p empty `shouldBe` Properties [p] []
      insert k  p empty `shouldBe` Properties [] [M.insert "a" (Properties [] [M.insert "b" (Properties [p] []) M.empty]) M.empty]
    it "Reject replacement" $ do
      insert k "1" m `shouldBe` m
    it "quickCheck" $ do
      quickCheck $ \a' (b :: Int)    -> let a = pack a' in Just b == (insert (toKeys a) (PNum $ fromIntegral b) empty) .>> a
      quickCheck $ \a' (b :: Bool)   -> let a = pack a' in Just b == (insert (toKeys a) (PBool b)               empty) .>> a
      quickCheck $ \a' (b :: String) -> let a = pack a' in Just b == (insert (toKeys a) (PStr $ pack b)         empty) .>> a
  context "lookup" $ do
    it "normal" $ do
      let m = insert ["a"] (PNum 1) empty
          n = insert ["a"] (PStr "true") empty
      (empty .>> "a" :: Maybe Int)    `shouldBe`   Nothing
      (m     .>> "a" :: Maybe Int)    `shouldBe`   Just  1
      (m     .>> "a" :: Maybe Bool)   `shouldFail` Nothing
      (m     .>> "a" :: Maybe String) `shouldBe`   Just "1.0"
      (n     .>> "a" :: Maybe Int)    `shouldFail` Nothing
      (n     .>> "a" :: Maybe Bool)   `shouldBe`   Just True
      (n     .>> "a" :: Maybe Text)   `shouldBe`   Just "true"
  context "makeProperties" $ do
    it "normal" $ do
      m <- makePropertiesFromEnvironment empty
      h <- getEnv "HOME"
      (m .>> "home") `shouldBe` Just h
  context "command-line" $ do
    it "normal" $ do
      let args = ["--package.a.enabled=true","--package.b.enabled=false","package.c.enabled=false"]
      m    <- makePropertiesFromCommandLine' args defaultParseCommandLine empty
      (m .>> "package.a.enabled") `shouldBe` Just True
      (m .>> "package.b.enabled") `shouldBe` Just False
      (m .>> "package.c.enabled" :: Maybe Bool )`shouldBe` Nothing
  context "environment" $ do
    it "normal" $ do
      let args = [("PACKAGE_A_ENABLED","true"),("PACKAGE_B_ENABLED","false")]
          m    = makePropertiesFromEnvironment' args empty
      (m .>> "package.a.enabled") `shouldBe` Just True
      (m .>> "package.b.enabled") `shouldBe` Just False
      (m .>> "package.c.enabled" :: Maybe Bool )`shouldBe` Nothing

specProperties = do
  context "defaultPropertiesWithFile" $ do
    let confName = "salak.yml" :: String
        confName' = pack confName
    it "read config" $ do
      unsetEnv "SALAK_CONFIG_NAME"
      p <- defaultPropertiesWithFile confName
      p .>> "salak.config.name" `shouldBe` Just confName
    it "read config - replacement" $ do
      setEnv "SALAK_CONFIG_NAME" confName
      p <- defaultPropertiesWithFile "salak.ok"
      p .>> "salak.config.name" `shouldBe` Just confName
    it "read config - not found" $ do
      setEnv "SALAK_CONFIG_NAME" "salak.notfound"
      setEnv "SALAK_CONFIG_DIR" "test"
      defaultPropertiesWithFile confName `shouldThrow` anyException
    it "read config - read file parse Config" $ do
      setEnv "SALAK_CONFIG_NAME" confName
      setEnv "SALAK_CONFIG_DIR" "test"
      setEnv "SALAK_CONFIG" confName
      p <- defaultPropertiesWithFile confName
      (p .>> "salak.config.name")              `shouldBe`   Just confName
      (p .>> "salak.config")                   `shouldBe`   Just confName
      (p .>> "salak.config" :: Maybe Config)   `shouldBe`   Just (Config confName' "test" 1)
      (p .>> "array"        :: Maybe [String]) `shouldBe`   Just ["a","b"]
      (p .>> "array"        :: Maybe [Int])    `shouldFail` Nothing
      (p .>> "array"        :: Maybe String)   `shouldFail` Nothing




