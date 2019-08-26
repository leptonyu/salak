module Salak.Internal.KeySpec(spec) where

import           Data.List       (intercalate)
import           Data.Text       (Text, pack, unpack)
import           Salak.Internal
import           Test.Hspec
import           Test.QuickCheck

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


spec :: SpecWith ()
spec = do
  context "selectors" $ do
    it "normal" $ do
      toKeys (""         :: Text) `shouldBe` Right mempty
      toKeys ("."        :: Text) `shouldBe` Right mempty
      toKeys (".."       :: Text) `shouldBe` Right mempty
      toKeys ("xx"       :: Text) `shouldBe` Right (fromKeys $ [KT "xx"])
      toKeys ("xx[0]"    :: Text) `shouldBe` Right (fromKeys $ [KT "xx", KI 0])
      toKeys ("xx.yy"    :: Text) `shouldBe` Right (fromKeys $ [KT "xx", KT "yy"])
      toKeys ("xx[0][1]" :: Text) `shouldBe` Right (fromKeys $ [KT "xx", KI 0, KI 1])
      show (fromKeys $ KT "x" : (KI <$> [0..9])) `shouldBe` "x[0][1][2][3][4][5][6][7][8][9]"
    it "QuickCheck" $ do
      quickCheck $ \s -> let s' = unKey s in (show <$> toKeys s') `shouldBe` Right (unpack s')
