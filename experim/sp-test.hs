module Main ( main ) where

import System.IO
import System.Environment
import Data.Char
import Foreign
import Control.Arrow.SP
import Control.Monad.State
import Data.List

----- Lazy-I/O version

revLazy :: IO ()
revLazy = interact (unlines . map reverse . lines)

----- SP version

toChar :: SP Word8 Char
toChar = pure (toEnum . fromEnum)

toLine :: SP Char String
toLine = getSP (tol [])
  where
  tol :: String -> Char -> SP Char String
  tol acc '\n' = putSP (reverse acc) toLine
  tol acc c    = getSP (tol (c:acc))

revLine :: SP String String
revLine = pure reverse

revLines :: SP Word8 String
revLines = toChar >>> toLine >>> revLine

bufsize :: Int                  -- our I/O buffer size
bufsize = 4096

readerSP :: Handle -> SP () Word8
readerSP h = ioSP $
  allocaArray bufsize $ \ptr -> do
    rc <- hGetBuf h ptr bufsize
    if rc == 0
       then return zeroArrow
       else do
         buf <- peekArray rc ptr :: IO [Word8]
         return (pushSP buf (readerSP h))

writeLine :: SP String ()
writeLine = getSP write
  where
  write :: String -> SP String ()
  write s = ioSP (putStrLn s >> return writeLine)

revSP :: IO ()
revSP = runSP sp >> return ()
  where sp = readerSP stdin >>> revLines >>> writeLine

----- Alternate IO driver

ioDriver :: Handle -> SP Word8 () -> IO ()
ioDriver hin initSP = allocaArray bufsize (ioLoop initSP)
  where
  ioLoop :: SP Word8 () -> Ptr Word8 -> IO ()
  ioLoop f ptr = do
    rc <- hGetBuf hin ptr bufsize
    if rc == 0
       then return ()
       else spLoop f ptr rc >>= \f' -> ioLoop f' ptr

  spLoop :: SP Word8 () -> Ptr Word8 -> Int -> IO (SP Word8 ())
  spLoop (Put () f) ptr n = spLoop f ptr n
  spLoop (SPIO f)   ptr n = f >>= \f' -> spLoop f' ptr n
  spLoop sp@(Get _)  _  0 = return sp
  spLoop (Get f)    ptr n = do sp' <- peek ptr >>= runSP . f
                               spLoop sp' (ptr `plusPtr` 1) (n - 1)

revSP' :: IO ()
revSP' = ioDriver stdin (revLines >>> writeLine)

----- Command-line driver

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--lazy"   : _ -> revLazy
    "--sp"     : _ -> revSP
    "--sp'"    : _ -> revSP'
    _              -> error "usage: --{bio,stream,lazy}"


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ignore-package streamproc -Wall" ***
-- End: ***
