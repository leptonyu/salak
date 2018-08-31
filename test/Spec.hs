{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Aeson
import qualified Data.HashMap.Strict    as M
import           Data.Maybe
import           Data.Salak
import           Data.Salak.CommandLine
import           Data.Salak.Environment
import qualified Data.Salak.Types       as P
import           Data.Text              (Text, intercalate, pack)
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck

main = hspec spec

spec :: Spec
spec = do
  describe "Data.Salak.Types" specProperty
  describe "Data.Salak"       specProperties

shouldFail :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
shouldFail f a = (f `shouldBe` a) `shouldThrow` anyErrorCall

data Config = Config
  { name :: Text
  , dir  :: Text
  , ext  :: Int
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
        <$> v .: "name"
        <*> v .: "dir"
        <*> (fromMaybe 1 <$> v .:? "ext")

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
      quickCheck $ \a' (b :: Integer) -> let a = pack a' in Just b == P.lookup a (insert (toKeys a) (PNum $ fromInteger b) empty)
      quickCheck $ \a' (b :: Bool)    -> let a = pack a' in Just b == P.lookup a (insert (toKeys a) (PBool b) empty)
      quickCheck $ \a' (b :: String)  -> let a = pack a' in Just b == P.lookup a (insert (toKeys a) (PStr $ pack b) empty)
  context "lookup" $ do
    it "normal" $ do
      let m = insert ["a"] (PNum 1) empty
          n = insert ["a"] (PStr "true") empty
      (P.lookup "a" empty :: Maybe Int)    `shouldBe`   Nothing
      (P.lookup "a" m     :: Maybe Int)    `shouldBe`   Just  1
      (P.lookup "a" m     :: Maybe Bool)   `shouldFail` Nothing
      (P.lookup "a" m     :: Maybe String) `shouldBe`   Just "1.0"
      (P.lookup "a" n     :: Maybe Int)    `shouldFail` Nothing
      (P.lookup "a" n     :: Maybe Bool)   `shouldBe`   Just True
      (P.lookup "a" n     :: Maybe Text)   `shouldBe`   Just "true"
  context "makeProperties" $ do
    it "normal" $ do
      m <- makePropertiesFromEnvironment empty
      h <- getEnv "HOME"
      (P.lookup "home" m) `shouldBe` Just h
  context "command-line" $ do
    it "normal" $ do
      let args = ["--package.a.enabled=true","--package.b.enabled=false","package.c.enabled=false"]
      m    <- makePropertiesFromCommandLine' args defaultParseCommandLine empty
      P.lookup "package.a.enabled" m `shouldBe` Just True
      P.lookup "package.b.enabled" m `shouldBe` Just False
      (P.lookup "package.c.enabled" m :: Maybe Bool )`shouldBe` Nothing
  context "environment" $ do
    it "normal" $ do
      let args = [("PACKAGE_A_ENABLED","true"),("PACKAGE_B_ENABLED","false")]
          m    = makePropertiesFromEnvironment' args empty
      P.lookup "package.a.enabled" m `shouldBe` Just True
      P.lookup "package.b.enabled" m `shouldBe` Just False
      (P.lookup "package.c.enabled" m :: Maybe Bool )`shouldBe` Nothing

specProperties = do
  context "defaultPropertiesWithFile" $ do
    let confName = "salak.yml" :: String
        confName' = pack confName
    it "read config" $ do
      unsetEnv "SALAK_CONFIG_NAME"
      p <- defaultPropertiesWithFile confName'
      P.lookup "salak.config.name" p `shouldBe` Just confName
    it "read config - replacement" $ do
      setEnv "SALAK_CONFIG_NAME" confName
      p <- defaultPropertiesWithFile "salak.ok"
      P.lookup "salak.config.name" p `shouldBe` Just confName
    it "read config - not found" $ do
      setEnv "SALAK_CONFIG_NAME" "salak.notfound"
      setEnv "SALAK_CONFIG_DIR" "test"
      defaultPropertiesWithFile confName' `shouldThrow` anyErrorCall
    it "read config - read file parse Config" $ do
      setEnv "SALAK_CONFIG_NAME" confName
      setEnv "SALAK_CONFIG_DIR" "test"
      setEnv "SALAK_CONFIG" confName
      p <- defaultPropertiesWithFile confName'
      let get :: FromProperties a => Text -> Maybe a
          get = flip P.lookup p
      -- print p
      -- let v::Return Value = fromProperties p
      -- P.fromReturn (return ()) $ BC.putStrLn . encodePretty <$> v
      get "salak.config.name"                `shouldBe`   Just confName
      get "salak.config"                     `shouldBe`   Just confName
      (get "salak.config" :: Maybe Config)   `shouldBe`   Just (Config confName' "test" 1)
      (get "array"        :: Maybe [String]) `shouldBe`   Just ["a","b"]
      (get "array"        :: Maybe [Int])    `shouldFail` Nothing
      (get "array"        :: Maybe String)   `shouldFail` Nothing




