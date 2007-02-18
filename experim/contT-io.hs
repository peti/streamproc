module Main where

import Prelude hiding (catch)
import System.IO
import Foreign
import Foreign.C
import Foreign.Marshal
import Control.Monad.Cont
import Control.Monad.Error
import Control.Exception
import Data.Char
import Data.Monoid
import Data.List

-- A FIO computation represents a value of type @r@ waiting to be
-- processed.

type FIO r = ContT r IO

-- Run an 'IO' computation in 'FIO'.

io :: IO a -> FIO r a
io = lift

-- Obtain the result of a 'FIO' in the 'IO' monad.

fio :: FIO st st -> IO st
fio f = runContT f return

shiftT :: Monad m => ((a -> ContT r m s) -> ContT s m s) -> ContT s m a
shiftT e = ContT $ \k -> e (lift . k) `runContT` return

resetT :: Monad m => ContT a m a -> ContT r m a
resetT e = lift $ e `runContT` return

alloc :: IO a                   -- construct a
      -> (a -> IO ())           -- destruct a
      -> FIO r a                -- a waiting for computation to yield r
alloc c d = withContT (\f _ -> bracket c d f) (return ())

check_order :: FIO () ()
check_order = alloc cons dest >> io (print "foo")
  where cons   = print "before foo"
        dest _ = print "after foo"

buffer :: Int -> FIO r (Ptr Word8)
buffer n = withContT (flip allocaArray) (return n)

spew :: Handle -> String -> FIO r ()
spew h = io . hPutStr h


-- Read a chunk and pass it on. @(nullPtr, 0)@ signals EOF.

readBlock :: Handle -> FIO r CStringLen
readBlock h = do
  let blocksize = 1024
  eof <- io (hWaitForInput h (-1))
  case eof of
    True  -> return (nullPtr, 0)
    False -> do
      p <- buffer blocksize
      i <- io (hGetBuf h p blocksize)
      case i <= 0 of
        True  -> return (nullPtr, 0)
        False -> return (castPtr p, i)

toString = withContT (\f buf -> peekCStringLen buf >>= f)

o :: (a -> b) -> (b -> c) -> a -> c
o a b = b . a

claus = readBlock `o` toString


main :: IO ()
main = fio $ readBlock stdin >>= io . peekCStringLen >>= spew stdout
