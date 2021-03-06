module Main where

import           Control.Monad.Reader
import           Data.List            (intercalate)
import           Data.Text            (Text, pack)
import           GHC.Generics
import           Salak
import           Salak.Internal
import           Salak.Toml
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Salak.Toml"  tomlProperty

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

instance FromProp m SubConf where
  fromProp = SubConf <$> "hello" .?= "yyy"

instance FromProp m Conf

tomlProperty :: SpecWith ()
tomlProperty = do
  context "load toml" $ do
    it "salak.toml" $ do
      loadAndRunSalak (loadToml "salak.toml") $ do
        SourcePack{..}  <- ask
        cf <- require "me.icymint.conf"
        lift $ do
          name cf `shouldBe` "shelly"
          age  cf `shouldBe` 16
          male cf `shouldBe` False
          det  cf `shouldBe` SubConf "def"





