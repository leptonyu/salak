module Salak.Types.Value where

import           Control.Monad.Writer
import qualified Data.PQueue.Min      as Q
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)
import           Data.Time

type Priority = Int

data Value
  = VStr  Priority !Text
  | VNum  Priority !Scientific
  | VBool Priority !Bool
  | VDate Priority !DateValue
  deriving Eq

instance Ord Value where
  compare a b = compare (getPriority a) (getPriority b)

instance Show Value where
  show (VStr  a b) = showV a b "Str"
  show (VNum  a b) = showV a b "Num"
  show (VBool a b) = showV a b "Bool"
  show (VDate a b) = showV a b "Date"

showV a b t = show b <> ":" <> t <> "#" <> show a

data DateValue
  = DV1 !TimeZone !LocalTime
  | DV2 !LocalTime
  | DV3 !Day
  | DV4 !TimeOfDay
  deriving (Eq, Show, Ord)

getPriority :: Value -> Priority
getPriority (VStr  p _) = p
getPriority (VNum  p _) = p
getPriority (VBool p _) = p
getPriority (VDate p _) = p

type QV = Q.MinQueue Value

getQ :: QV -> Maybe Value
getQ = Q.getMin

nullQ :: QV -> Bool
nullQ = Q.null

insertQ :: Value -> QV -> QV
insertQ = Q.insert

replaceQ :: Monad m => String -> Priority -> QV -> QV -> WriterT [String] m QV
replaceQ s i nq q = do
  let (a,b) = Q.partition ((==i) . getPriority) q
      go v  = tell $ (\vi -> "#" <> show i <> " " <> vi) <$> v
  if a == nq
    then return q
    else case getQ nq of
      Just v -> do
        go [(if Q.null a then "Add " else "Mod ") ++ s]
        return $ Q.insert v b
      _      -> do
        unless (Q.null a) $ go ["Del " ++ s]
        return b
