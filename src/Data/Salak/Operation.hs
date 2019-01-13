module Data.Salak.Operation where

import           Data.Salak.Types
import           Data.Text        (Text, unpack)

infixl 5 .?>
(.?>) :: FromProperties a => Properties -> Text -> Return a
(.?>) = flip lookup'

infixl 5 .|=
(.|=) :: Return a -> a -> a
(.|=) (Right a)       _ = a
(.|=) (Left (Fail e)) _ = error e
(.|=) _               d = d

infixl 5 .?=
(.?=) :: Return a -> a -> Return a
(.?=) a b = Right (a .|= b)

infixl 5 .>>
(.>>) :: FromProperties a => Properties -> Text -> a
(.>>) p key = case p .?> key of
  Right v           -> v
  Left (EmptyKey k) -> error $ "Config " <> unpack k <> " not found"
  Left (Fail e)     -> error e
