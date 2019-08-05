module Salak.Internal.Writable(
  -- ** Writable Value
    Writable
  , toWritable
  , getWritable
  , setWritable
  ) where

import           Control.Concurrent.MVar
import           Control.Monad

-- | Writable data structure. `Writable` is designed for working with `IO` value pased by salak.
-- It provide a way to override `IO` value provided by salak, can be used in the application which need to change
-- values of some configurations by overriding it directly. For example, logger function can use a log level property
-- to control which level of logs should be printed. By using `Writeable` value, we can change the property
-- directly.
data Writable a = Writable
  { valRef :: IO a
  , setRef :: MVar (Maybe a)
  }

-- | Convert a `IO` value to `Writable` value.
toWritable :: IO a -> IO (Writable a)
toWritable valRef = do
  setRef <- newMVar Nothing
  return Writable{..}

-- | Get value.
getWritable :: Writable a -> IO a
getWritable Writable{..} = do
  v <- readMVar setRef
  case v of
    Just a -> return a
    _      -> valRef

-- | Set or remove override value.
setWritable :: Maybe a -> Writable a -> IO ()
setWritable s Writable{..} = void $ swapMVar setRef s
