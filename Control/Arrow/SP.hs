{- |
   Module      :  Control.Arrow.SP
   Copyright   :  (c) 2004-12-18 by Shawn Garbett and Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   A continuation-based stream processor implemented as an
   'Arrow'.

   References:

   * Magnus Carlsson, Thomas Hallgren, \"/Fudgets--Purely
     Functional Processes with applications to Graphical
     User Interfaces/\":
     <http://www.cs.chalmers.se/~hallgren/Thesis/>

   * John Hughes, \"/Generalising Monads to Arrows/\":
     <http://www.cs.chalmers.se/~rjmh/Papers/arrows.pdf>
-}

module Control.Arrow.SP
  ( SP(..)
  , runSP
  , putSP, getSP, ioSP, popSP, pushSP
  , listToSP, spToList
  , idSP, mapSP, filterSP, testSP, stateSP
  , module Control.Arrow
  )
  where

import Control.Arrow
import Control.Monad ( liftM )

-- |A generic stream processor.

data SP i o
  = Put o (SP i o)
  | Get (i -> SP i o)
  | SPIO (IO (SP i o))

instance Arrow SP where
  -- Define sequencing operator.
  Put i sp1 >>> Get sp2   = sp1 >>> sp2 i
  sp1       >>> Put o sp2 = Put o (sp1 >>> sp2)
  Get sp1   >>> Get sp2   = Get (\i -> sp1 i >>> Get sp2)
  -- Downstream IO is processed once puts/gets settle
  -- waiting on IO.
  sp        >>> SPIO spio = SPIO (liftM (sp >>>) spio)
  -- Upstream IO is processed last.
  SPIO spio >>> sp        = SPIO (liftM (>>> sp) spio)
  -- Define arrow operator (equivalent to return in a monad).
  arr f   = Get (\x -> Put (f x) (arr f))
  -- first is for parallel output in pairs.
  first f = bypass [] f

-- ArrowZero just waits in a state getting input forever.

instance ArrowZero SP where
  zeroArrow = Get (\_ -> zeroArrow)

-- ArrowPlus allows running in parallel, output merged into
-- a single stream.

instance ArrowPlus SP where
  Put o sp1  <+> sp2       = Put o (sp1 <+> sp2)
  sp1        <+> Put o sp2 = Put o (sp1 <+> sp2)
  Get sp1    <+> Get sp2   = Get (\i -> sp1 i <+> sp2 i )
  sp1        <+> SPIO spio = SPIO (liftM (sp1 <+>) spio)
  SPIO spio  <+> sp2       = SPIO (liftM (<+> sp2) spio)

-- Left messages pass through like a conduit. Right messages
-- are processed by the SP.

instance ArrowChoice SP where
  left (Put c sp)  = Put (Left c) (left sp)
  left (SPIO spio) = SPIO (liftM left spio)
  left (Get f)     = Get $ \z ->
    case z of
      Left a   -> left (f a)
      Right b  -> Put (Right b) (left (Get f))

-- A feedback loop where a SP can examine it's own output.

instance ArrowLoop SP where
  loop sp = loop' empty sp
    where
    loop' :: Queue c -> SP (a,c) (b,c) -> SP a b
    loop' q (SPIO spio)     = SPIO (liftM (loop' q) spio)
    loop' q (Put (a,b) sp') = Put a (loop' (join b q) sp')
    loop' q (Get sp')       =
      case qremove q of
        Just (i, q') -> Get (\x -> loop' q' (sp' (x,i)))
        Nothing      -> error "SP feedback queue empty"

-- |Evaluate a stream processor in the IO Monad.

runSP :: SP i o -> IO (SP i o)
runSP (Put x f)   = runSP f >>= return . Put x
runSP (SPIO spio) = spio >>= runSP
runSP sp          = return sp

-- |Read any output the SP has produced and return it along
-- with the \"new\" SP.

popSP :: SP i o -> ([o], SP i o)
popSP sp = pop [] sp
  where
  pop acc (Put x xs) = pop (x : acc) xs
  pop acc xs         = (reverse acc, xs)

-- |'putSP' the contents of the list, then behave like the
-- given SP. Similar to a push on a stack.

pushSP :: [o] -> SP i o -> SP i o
pushSP [] sp     = sp
pushSP (x:xs) sp = putSP x $ pushSP xs sp

-- |Return any output the SP has produced as a list.

spToList :: SP i o -> [o]
spToList = fst . popSP

-- |Convert a list into a SP: @listToSP x = pushSP x zeroArrow@

listToSP :: [o] -> SP i o
listToSP x = pushSP x zeroArrow

-- |The identity SP.

idSP :: SP a a
idSP = Get $ \x -> Put x idSP

-- |Create an SP which puts a value into the stream, then
-- behaves like the given SP.

putSP :: b -> SP a b -> SP a b
putSP = Put

-- |Wait for input and process it with the given SP.

getSP :: (a -> SP a b) -> SP a b
getSP = Get

-- |Lift an I\/O computation into a SP.

ioSP :: IO (SP a b) -> SP a b
ioSP = SPIO

-- |Construct a 'map'ping SP from a pure function.

mapSP :: (a-> b) -> SP a b
mapSP f = Get $ \x -> Put (f x) $ mapSP f

-- |Construct a 'filter'ing SP from a pure function.

filterSP :: (a -> Bool) -> SP a a
filterSP p =
  getSP $ \x ->
    if p x
       then Put x (filterSP p)
       else filterSP p

-- |Create a stateful SP using a state transition function.

stateSP :: (s -> i -> (s, [o])) -> s -> SP i o
stateSP f s0 =
  getSP $ \x ->
    let (s, ys) = f s0 x
    in
    pushSP ys (stateSP f s)

-- |If the given arrow returns 'True', tag the output value
-- as 'Left', otherwise tag it as 'Right'.

testSP :: Arrow a => a b Bool -> (a b (Either b b))
testSP f =
  (f &&& returnA) >>> arr (\(b,x) -> if b then Left x else Right x)

----- Helper Functions -----------------------------------------------

bypass :: [c] -> SP a b -> SP (a,c) (b,c)
bypass ds     (Get f')    = Get (\(b,d) -> bypass (ds ++ [d]) (f' b))
bypass (d:ds) (Put c sp)  = Put (c,d) (bypass ds sp)
bypass []     (Put c sp)  = Get (\(_,d) -> Put (c,d) (bypass [] sp))
bypass ds     (SPIO spio) = SPIO (spio >>= (\sp -> return (bypass ds sp)))

data Queue a = Queue [a] [a]
             deriving (Show)

empty :: Queue a
empty = Queue [] []

join :: a -> Queue a -> Queue a
join x (Queue o i) = Queue o (x:i)

qremove :: Queue a -> Maybe (a, Queue a)
qremove (Queue (o:os) i) = Just (o,(Queue os i))
qremove (Queue []    []) = Nothing
qremove (Queue []     i) = qremove (Queue (reverse i) [])


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ignore-package streamproc -Wall" ***
-- End: ***
