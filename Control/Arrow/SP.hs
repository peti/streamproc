{- |
   Module      :  Control.Arrow.SP
   Copyright   :  (c) 2007 by Shawn Garbett and Peter Simons
   License     :  BSD3

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable

   A continuation-based monadic stream processor implemented as
   an 'Arrow'.

   References:

   * John Hughes, \"/Generalising Monads to Arrows/\":
     <http://www.ittc.ku.edu/Projects/SLDG/filing_cabinet/Hughes_Generalizing_Monads_to_Arrows.pdf>

   * Magnus Carlsson, Thomas Hallgren, \"Fudgets--Purely
     Functional Processes with applications to Graphical
     User Interfaces\":
     <http://www.cs.chalmers.se/~hallgren/Thesis/>
-}

module Control.Arrow.SP
  ( SP(..), runSP, mapSP
  , module Control.Arrow
  )
  where

import Prelude hiding ( id, (.) )
import Control.Category
import Control.Arrow
import Control.Monad ( liftM )

-- |A generic stream processor.

data SP m i o = Put o (SP m i o)
              | Get (i -> SP m i o)
              | Block (m (SP m i o))

instance Monad m => Category (SP m) where
  id                      = Get (`Put` id)
  (Get sp2) . (Put i sp1) = sp1 >>> sp2 i
  (Put o sp2) . sp1       = Put o (sp1 >>> sp2)
  (Get sp2) . (Get sp1)   = Get (\i -> sp1 i >>> Get sp2)
  (Block spm) . sp        = Block (fmap (sp >>>) spm)
  sp . (Block spm)        = Block (fmap (>>> sp) spm)

instance Monad m => Arrow (SP m) where
  arr f                   = Get (\x -> Put (f x) (arr f))
  first                   = bypass empty
    where
      bypass :: Monad m => Queue c -> SP m a b -> SP m (a,c) (b,c)
      bypass q (Get f)     = Get (\(a,c) -> bypass (push c q) (f a))
      bypass q (Block spm) = Block (fmap (bypass q) spm)
      bypass q (Put c sp)  = case pop q of
                                Just (c', q') -> Put (c,c') (bypass q' sp)
                                Nothing       -> Get (\(_,d) -> Put (c,d) (bypass q sp))

-- ArrowZero just waits in a state getting input forever.

instance Monad m => ArrowZero (SP m) where
  zeroArrow = Get (const zeroArrow)

-- ArrowPlus allows running in parallel, output merged into
-- a single stream.

instance Monad m => ArrowPlus (SP m) where
  Put o sp1  <+> sp2        = Put o (sp1 <+> sp2)
  sp1        <+> Put o sp2  = Put o (sp1 <+> sp2)
  Get sp1    <+> Get sp2    = Get (\i -> sp1 i <+> sp2 i )
  sp1        <+> Block spm  = Block (fmap (sp1 <+>) spm)
  Block spm  <+> sp2        = Block (fmap (<+> sp2) spm)

-- Left messages pass through like a conduit. Right messages
-- are processed by the SP.

instance Monad m => ArrowChoice (SP m) where
  left (Put c sp)  = Put (Left c) (left sp)
  left (Block spm) = Block (fmap left spm)
  left (Get f)     = Get (either (left . f) (\b -> Put (Right b) (left (Get f))))

-- A feedback loop where a SP can examine it's own output.

instance Monad m => ArrowLoop (SP m) where
  loop = loop' empty
    where
    loop' :: Monad m => Queue c -> SP m (a,c) (b,c) -> SP m a b
    loop' q (Block spm)     = Block (fmap (loop' q) spm)
    loop' q (Put (a,b) sp') = Put a (loop' (push b q) sp')
    loop' q (Get sp')       = case pop q of
                                Just (i, q') -> Get (\x -> loop' q' (sp' (x,i)))
                                Nothing      -> Block (fail "invalid attempt to consume empty SP feedback loop")

-- |Evaluate a stream processor.

runSP :: Monad m => SP m () () -> m ()
runSP (Block spm)  = spm >>= runSP
runSP (Put () f)   = runSP f
runSP (Get _)      = return ()

-- |Use a monadic transformer to map a stream.

mapSP :: (Monad m) => (i -> m o) -> SP m i o
mapSP f = Get (\i -> Block (f i >>= \o -> return (Put o (mapSP f))))

----- Helper Functions -----------------------------------------------

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue o i) = Queue o (x:i)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (o:os) i) = Just (o, Queue os i)
pop (Queue []    []) = Nothing
pop (Queue []     i) = pop (Queue (reverse i) [])
