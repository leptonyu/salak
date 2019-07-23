{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Writer
import           Data.Either
import           Data.List             (intercalate)
import           Data.Menshen
import           Data.Text             (Text, pack, unpack)
import           GHC.Generics
import           Salak
import           Salak.Internal
import           Salak.Internal.Key
import           Salak.Internal.Prop
import           Salak.Internal.Source
import           Salak.Internal.Val
import qualified Salak.Trie            as T
import           System.Random         (randomIO)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
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

specProperty :: SpecWith ()
specProperty = do
  context "selectors" $ do
    it "normal" $ do
      toKeys (""         :: Text) `shouldBe` Right (Keys $ [])
      toKeys ("."        :: Text) `shouldBe` Right (Keys $ [])
      toKeys (".."       :: Text) `shouldBe` Right (Keys $ [])
      toKeys ("xx"       :: Text) `shouldBe` Right (Keys $ [KT "xx"])
      toKeys ("xx[0]"    :: Text) `shouldBe` Right (Keys $ [KT "xx", KI 0])
      toKeys ("xx.yy"    :: Text) `shouldBe` Right (Keys $ [KT "xx", KT "yy"])
      toKeys ("xx[0][1]" :: Text) `shouldBe` Right (Keys $ [KT "xx", KI 0, KI 1])
      show (Keys $ KT "x" : (KI <$> [0..9])) `shouldBe` "x[0][1][2][3][4][5][6][7][8][9]"
    it "QuickCheck" $ do
      quickCheck $ \s -> let s' = unKey s in (show <$> toKeys s') `shouldBe` Right (unpack s')
  -- context "value" $ do
  --   it "basic" $ do
  --     newVT "xxxx"       0 `shouldBe` VT 0 "xxxx"
  --     newVT "{x.y}"      0 `shouldBe` VT 0 "{x.y}"
  --     newVT "${x*}"      0 `shouldBe` VT 0 "${x*}"
      -- newVT "${x}"       0 `shouldBe` VRef 0 [RRef [KT "x"]]
      -- newVT "${x.y}"     0 `shouldBe` VRef 0 [RRef [KT "x", KT "y"]]
      -- newVT "${x${y}}"   0 `shouldBe` VT 0 "${x${y}}"
      -- newVT "a${x}b"     0 `shouldBe` VRef 0 [RVal "a", RRef [KT "x"], RVal "b"]
      -- newVT "a${x}b${"   0 `shouldBe` VRef 0 [RVal "a", RRef [KT "x"], RVal "b${"]
      -- newVT "a${x}b${c}" 0 `shouldBe` VRef 0 [RVal "a", RRef [KT "x"], RVal "b", RRef [KT "c"]]
  context "source" $ do
    it "normal - 2" $ do
      let (a,b,c) = extract T.empty $ Salak.Internal.Source.generate 0 ([("hello", "world")] :: [(Text, Text)])
      length c `shouldBe` 0
      length (T.toList b) `shouldBe` 1
  --   it "normal - 3" $ do
  --     let (s2,e2) = runWriter $ insert "1"     (VT 0 "world") emptySource
  --     sizeSource s2       `shouldBe` 1
  --     length e2           `shouldBe` 0
  --   it "normal - 4" $ do
  --     let (s3,e3) = runWriter $ insert "a.b"   (VT 0 "world") emptySource
  --     print s3
  --     sizeSource s3       `shouldBe` 1
  --     length e3           `shouldBe` 0
  -- context "source - merge" $ do
  --   let s  = fst $ runWriter $ insert "hello" (VT 0 "world") emptySource
  --       so = fst $ runWriter $ insert "hello" (VT 1 "yyyyy") emptySource
  --       s1 = fst $ runWriter $ insert "hello" (VT 0 "xxxxx") emptySource
  --   it "merge - del" $ do
  --     let (s2,e2) = runWriter $ replace 0 emptySource s
  --     s2 `shouldBe` emptySource
  --     e2 `shouldBe` ["#0 Del hello"]
  --     let (s3,e3) = runWriter $ replace 1 emptySource s
  --     s3 `shouldBe` s
  --     e3 `shouldBe` []
  --   it "merge - mod" $ do
  --     let (s3,e3) = runWriter $ replace 0 s1 s
  --     e3 `shouldBe` ["#0 Mod hello"]
  --     s3 `shouldBe` s1
  --     let (s2,e2) = runWriter $ replace 1 s1 s
  --     e2 `shouldBe` []
  --     s2 `shouldBe` s
  --     let (_,e4) = runWriter $ replace 1 so s
  --     e4 `shouldBe` ["#1 Add hello"]
  --   it "merge - add" $ do
  --     let (s4,e4) = runWriter $ replace 0 s emptySource
  --     e4 `shouldBe` ["#0 Add hello"]
  --     s4 `shouldBe` s
  --     let (s2,e2) = runWriter $ replace 1 s emptySource
  --     s2 `shouldBe` emptySource
  --     e2 `shouldBe` []
  --     let (s3,e3) = runWriter $ replace 1 so emptySource
  --     e3 `shouldBe` ["#1 Add hello"]
  --     s3 `shouldBe` so
  --   it "merge - unchange" $ do
  --     let (s5,e5) = runWriter $ replace 0 s s
  --     e5 `shouldBe` []
  --     s5 `shouldBe` s
  -- context "Generic" $ do
  --   it "conf" $ do
  --     sp <- runLoadT Nothing $ loadOnceMock
  --       [ ("name", "Daniel")
  --       , ("age", "18")
  --       , ("male", "yes")
  --       ]
  --     errs sp `shouldBe` []
  --     let a = search "" sp :: Either String Conf
  --     print a
  --     isRight a `shouldBe` True
  --   it "placeholder" $ do
  --     let xs = [ ("name", "daniel")
  --              , ("user", "${name}")
  --              , ("a","${b}")
  --              , ("b","${a}")
  --              , ("x", "${y}")
  --              , ("y", "${z}")
  --              , ("z", "Hey! you")
  --              ]
  --     loadAndRunSalak (loadOnceMock xs) $ do
  --       a <- require "name"
  --       b <- require "user"
  --       x <- require "x"
  --       z <- require "z"
  --       lift $ do
  --         a `shouldBe` (b :: Text)
  --         x `shouldBe` (z :: Text)
  --     let x = loadAndRunSalak (loadOnceMock xs) (require "a") :: IO Text
  --     x `shouldThrow` anyErrorCall
  -- context "Reload test" $ do
  --   it "reload" $ do
  --     (x :: IO Int,r) <- loadAndRunSalak (loadMock [("hello", pack . show <$> (randomIO :: IO Int))]) $ do
  --       v <- requireD "hello"
  --       a <- reloadAction
  --       return (v,a)
  --     a <- x
  --     ReloadResult{..} <- r
  --     isError  `shouldBe` False
  --     mapM_ print msg
  --     b <- x
  --     a  `shouldNotBe` b







