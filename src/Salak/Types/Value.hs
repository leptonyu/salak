module Salak.Types.Value where

import           Control.Monad.Writer
import qualified Data.PQueue.Min      as Q
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)
import           Data.Time

type Priority = Int

data Value
  = VStr   !Priority !Text
  | VNum   !Priority !Scientific
  | VBool  !Priority !Bool
  | VZTime !Priority !TimeZone !LocalTime
  | VLTime !Priority !LocalTime
  | VDay   !Priority !Day
  | VHour  !Priority !TimeOfDay
  deriving Eq

instance Ord Value where
  compare a b = compare (getPriority a) (getPriority b)

instance Show Value where
  show v = let (a,b,c) = typeOfV v in c <> ":" <> b <> "#" <> show a

typeOfV :: Value -> (Priority, String, String)
typeOfV (VStr   a b)   = (a, "Str",       show b)
typeOfV (VNum   a b)   = (a, "Num",       show b)
typeOfV (VBool  a b)   = (a, "Bool",      show b)
typeOfV (VZTime a b c) = (a, "ZonedTime", show (ZonedTime c b))
typeOfV (VLTime a b)   = (a, "LocalTime", show b)
typeOfV (VDay   a b)   = (a, "Day",       show b)
typeOfV (VHour  a b)   = (a, "TimeOfDay", show b)

getPriority :: Value -> Priority
getPriority x = let (a,_,_) = typeOfV x in a

getType :: Value -> String
getType x = let (_,b,_) = typeOfV x in b

getV :: Value -> String
getV x = let (_,_,b) = typeOfV x in b

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
