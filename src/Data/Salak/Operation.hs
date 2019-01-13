module Data.Salak.Operation where

import           Data.Salak.Types
import           Data.Text        (Text, unpack)

-- | Find `Properties` by key and convert to specific Haskell value.
--
-- @since 0.2.1
infixl 5 .?>
(.?>) :: FromProperties a => Properties -> Text -> Return a
(.?>) = flip Data.Salak.Types.lookup

-- | Get property or use default value if not found, but will throw
-- exception if parse failed.
--
-- @since 0.2.1
infixl 5 .|=
(.|=) :: Return a -> a -> a
(.|=) (Right a)       _ = a
(.|=) (Left (Fail e)) _ = error e
(.|=) _               d = d

-- | Use default value if Key not found
--
-- @since 0.2.1
infixl 5 .?=
(.?=) :: Return a -> a -> Return a
(.?=) a b = Right (a .|= b)

-- | Find `Properties` by key and convert to specific Haskell value.
-- Throw error if property not found or parse failed
--
-- @since 0.2.1
infixl 5 .>>
(.>>) :: FromProperties a => Properties -> Text -> a
(.>>) p key = case p .?> key of
  Right v           -> v
  Left (EmptyKey k) -> error $ "property " <> unpack k <> " not set"
  Left (Fail e)     -> error e
