module RingBuffers.Unlifted
  ( RingBuffer
  , new
  , clear
  , append
--  ,concat
  , capacity
  , filledLength
  , latest
  , foldMap
  ) where

import qualified RingBuffers.Internal as I

-- | A concurrent, mutable ring buffer that supports atomic updates.
newtype RingBuffer a = RingBuffer (I.RingBuffer UnliftedArray a)

-- | Return a new ring buffer of the specified size.
new :: (PrimUnlifted a)
  => Int -- ^ capacity of buffer
  -> IO (RingBuffer a)
new sz = fmap coerce (I.new sz)

-- | Reset the buffer to its empty state.
clear :: (PrimUnlifted a)
  => RingBuffer a -- ^ buffer to clear
  -> IO ()
clear rb = I.clear (coerce rb)

-- | Get the current filled length of the ring
filledLength :: (PrimUnlifted a)
  => RingBuffer a
  -> IO Int
filledLength rb = I.filledLength (coerce rb)

-- | Get the maximum number of items the ring can contain
capacity :: (PrimUnlifted a)
  => RingBuffer a
  -> IO Int
capacity rb = I.capacity (coerce rb)

-- | Retrieve the \(n\)th most-recently added item of the ring
latest :: (PrimUnlifted a)
  => RingBuffer a
  -> Int
  -> IO (Maybe a)
latest rb n = I.latest (coerce rb) n

-- | Add an item to the end of the buffer.
append :: (PrimUnlifted a)
  => a
  -> RingBuffer a
  -> IO ()
append x rb = I.append x (coerce rb)

-- | Execute the given action with the items of the ring, accumulating its results.
-- 
foldMap :: (PrimUnlifted a, Monoid b)
  => RingBuffer a
  -> (a -> IO b)
  -> IO b
foldMap rb action = I.foldMap (coerce rb) action

{-
-- | Operate atomically on a buffer.
withRing :: ()
  => RingBuffer a -- ^ buffer to operate on
  -> (MutableArray RealWorld a -> RingState -> IO (RingState, r)) -- ^ function that takes a buffer, a ring state, and returns a new ring state with a value.
  -> IO r
withRing rb f = I.withRing (coerce rb) f

-- | Advance the ring buffer's state by the given number of elements
advance :: ()
  => Int
  -> (MutableArray RealWorld a -> RingState -> IO (RingState, ()))
advance n = I.advance n
-}
