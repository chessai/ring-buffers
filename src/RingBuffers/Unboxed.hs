module RingBuffers.Unboxed
  ( RingBuffer
  , new
  , clear
  , append
  , extend
  , capacity
  , filledLength
  , latest
  , unsafeLatest
  , foldMap
  ) where

import qualified RingBuffers.Internal as I

-- | A concurrent, mutable ring buffer that supports atomic updates.
newtype RingBuffer a = RingBuffer (I.RingBuffer PrimArray a)

-- | Return a new ring buffer of the specified size.
new :: (Prim a)
  => Int -- ^ capacity of buffer
  -> IO (RingBuffer a)
new sz = fmap coerce (I.new sz)

-- | Reset the buffer to its empty state.
clear :: (Prim a)
  => RingBuffer a -- ^ buffer to clear
  -> IO ()
clear rb = I.clear (coerce rb)

-- | Get the current filled length of the ring
filledLength :: (Prim a)
  => RingBuffer a
  -> IO Int
filledLength rb = I.filledLength (coerce rb)

-- | Get the maximum number of items the ring can contain
capacity :: (Prim a)
  => RingBuffer a
  -> IO Int
capacity rb = I.capacity (coerce rb)

-- | Retrieve the \(n\)th most-recently added item of the ring
latest :: (Prim a)
  => RingBuffer a
  -> Int
  -> IO (Maybe a)
latest rb n = I.latest (coerce rb) n

-- | Retrieve the \(n\)th most-recently added item of the ring
--
--   /Note/: This function may exhibit undefined behaviour if
--   the index is out-of-bounds or uninitialised.
unsafeLatest :: (Prim a)
  => RingBuffer a
  -> Int
  -> IO a
unsafeLatest rb n = I.unsafeLatest (coerce rb) n

-- | Add an item to the end of the buffer.
append :: (Prim a)
  => a
  -> RingBuffer a
  -> IO ()
append x rb = I.append x (coerce rb)

-- | Write multiple items to the end of the ring.
--
--   Ignores any elements of the input array whose indices
--   are higher than the length of the ring buffer.
extend :: (Prim a)
  => PrimArray a
  -> RingBuffer a
  -> IO ()
extend x rb = I.extend x (coerce rb)

-- | Execute the given action with the items of the ring, accumulating its results.
--
foldMap :: (Prim a, Monoid b)
  => RingBuffer a
  -> (a -> IO b)
  -> IO b
foldMap rb action = I.foldMap (coerce rb) action
