module Salak.Internal.TrieSpec where

import qualified Data.HashMap.Strict as HM
import           Salak.Internal.Key
import qualified Salak.Trie          as T
import           Test.Hspec


spec :: SpecWith ()
spec = do
  context "Trie" $ do
    it "insert" $ do
      let Right keys = toKeys ("a.b.c" :: String)
          v          = T.insert keys (1 :: Int) T.empty
          w          = T.alter (\_ -> Just 1) keys T.empty
      v `shouldBe` w
      T.getPrimitive (T.subTries keys v) `shouldBe` Just 1
    it "toList" $ do
      let Right keys = toKeys ("a.b.c.d" :: String)
          w          = T.insert keys (2 :: Int) T.empty
          [(k,v)]    = T.toList w
      k `shouldBe` keys
      v `shouldBe` 2
      HM.member (KT "a") (T.getMap w) `shouldBe` True
