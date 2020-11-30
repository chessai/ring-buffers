-- | A concurrent, mutable ring buffer that supports atomic updates. This module is most efficient on buffers containing unboxed types.
module RingBuffers.Unlifted
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
  , toList
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

-- | Retrieve the \(n\)th most-recently added item of the ring
--
--   /Note/: This function may exhibit undefined behaviour if
--   the index is out-of-bounds or uninitialised.
unsafeLatest :: (PrimUnlifted a)
  => RingBuffer a
  -> Int
  -> IO a
unsafeLatest rb n = I.unsafeLatest (coerce rb) n

-- | Add an item to the end of the buffer.
append :: (PrimUnlifted a)
  => a
  -> RingBuffer a
  -> IO ()
append x rb = I.append x (coerce rb)

-- | Write multiple items to the end of the ring.
--
--   Ignores any elements of the input array whose indices
--   are higher than the length of the ring buffer.
extend :: (PrimUnlifted a)
  => UnliftedArray a
  -> RingBuffer a
  -> IO ()
extend x rb = I.extend x (coerce rb)

-- | Execute the given action with the items of the ring, accumulating its results.
--
foldMap :: (PrimUnlifted a, Monoid b)
  => RingBuffer a
  -> (a -> IO b)
  -> IO b
foldMap rb action = I.foldMap (coerce rb) action

-- | Convert the entire contents of the ring into a list,
--   with the most recently added element at the head.
toList :: (PrimUnlifted a)
  => RingBuffer a
  -> IO [a]
toList rb = I.toList (coerce rb)
