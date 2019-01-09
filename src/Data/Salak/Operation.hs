module Data.Salak.Operation where

import           Data.Salak.Types
import           Data.Text        (Text, unpack)

infixl 5 .?>
(.?>) :: FromProperties a => Properties -> Text -> Return a
(.?>) = flip lookup'

infixl 5 .|=
(.|=) :: Return a -> a -> a
(.|=) (OK   a) _ = a
(.|=) (Fail e) _ = error e
(.|=) _        d = d

infixl 5 .?=
(.?=) :: Return a -> a -> Return a
(.?=) a b = OK (a .|= b)

infixl 5 .>>
(.>>) :: FromProperties a => Properties -> Text -> a
(.>>) p k = case p .?> k of
  OK v   -> v
  Empty  -> case fromProperties empty of
    (OK   a) -> a
    (Fail e) -> error e
    _        -> error $ "Config " <> unpack k <> " not found"
  Fail e -> error e
