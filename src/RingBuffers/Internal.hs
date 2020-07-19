{-# language BangPatterns #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}

module RingBuffers.Internal
  ( RingBuffer(..)
  , RingState(..)
  , withRing
  , new
  , clear
  , capacity
  , filledLength
  , latest
  , unsafeLatest
  , advance
  , extend
  , append
  , foldMap
  , toList
  ) where

import qualified Data.Primitive.Contiguous as Contiguous

data RingBuffer arr a = RingBuffer
  { _ringBufferBuffer :: !(Mutable arr RealWorld a)
  , _ringBufferState :: {-# UNPACK #-} !(MVar RingState)
  }

data RingState = RingState
  { _ringStateFull :: !Bool -- ^ Is the ring buffer full?
  , _ringStateHead :: !Int  -- ^ next entry to be written
  }

ringState0 :: RingState
ringState0 = RingState False 0
{-# inline ringState0 #-}

-- TODO use withMVar
withRing :: (Contiguous arr, Element arr a)
  => RingBuffer arr a
  -> (Mutable arr RealWorld a -> RingState -> IO (RingState, r))
  -> IO r
withRing (RingBuffer ba bs) f = do
  s <- takeMVar bs
  (s',r) <- f ba s
  putMVar bs s'
  pure r
{-# inline withRing #-}

new :: (Contiguous arr, Element arr a)
  => Int
  -> IO (RingBuffer arr a)
new !sz = do
  ba <- Contiguous.new sz
  s0 <- newMVar ringState0
  pure (RingBuffer ba s0)
{-# inlineable new #-}

clear :: (Contiguous arr, Element arr a)
  => RingBuffer arr a
  -> IO ()
clear rb = withRing rb $ \_ _ -> pure (ringState0,())
{-# inline clear #-}

capacity :: (Contiguous arr, Element arr a)
  => RingBuffer arr a
  -> IO Int
capacity (RingBuffer buf _) = Contiguous.sizeMutable buf
{-# inline capacity #-}

filledLength :: (Contiguous arr, Element arr a)
  => RingBuffer arr a
  -> IO Int
filledLength rb = withRing rb $ \_ rs@(RingState full pos) -> if full
  then do
    cap <- capacity rb
    pure (rs,cap)
  else pure (rs,pos)
{-# inline filledLength #-}

latest :: (Contiguous arr, Element arr a)
  => RingBuffer arr a
  -> Int
  -> IO (Maybe a)
latest rb n = do
  len <- filledLength rb
  if n >= len
    then pure Nothing
    else Just <$> unsafeLatest rb n
{-# inline latest #-}

unsafeLatest :: (Contiguous arr, Element arr a)
  => RingBuffer arr a
  -> Int
  -> IO a
unsafeLatest rb n = do
  cap <- capacity rb
  withRing rb $ \ba bs@(RingState _ hd) -> do
    let idx = (hd - n - 1) `mod` cap
    (bs,) <$> Contiguous.read ba idx

advance :: (Contiguous arr, Element arr a)
  => Int
  -> (Mutable arr RealWorld a -> RingState -> IO (RingState, ()))
advance n = \ba (RingState full pos) -> do
  cap <- Contiguous.sizeMutable ba
  let (a,pos') = (pos + n) `divMod` cap
  pure (RingState (full || a > 0) pos', ())
{-# inline advance #-}

append :: (Contiguous arr, Element arr a)
  => a
  -> RingBuffer arr a
  -> IO ()
append x rb = withRing rb $ \ba bs -> do
  Contiguous.write ba (_ringStateHead bs) x
  advance 1 ba bs
{-# inline append #-}

extend :: (Contiguous arr, Element arr a)
  => arr a
  -> RingBuffer arr a
  -> IO ()
extend xs rb = withRing rb $ \ba bs -> do
  cap <- capacity rb
  let extensionLength = min (Contiguous.size xs) cap
  let currentHead = _ringStateHead bs
  let go !ix = when (ix < extensionLength) $ do
        atIx <- Contiguous.indexM xs ix
        Contiguous.write ba (currentHead + ix) atIx
        go (ix + 1)
  go 0
  advance extensionLength ba bs
{-# inlineable extend #-}

foldMap :: (Contiguous arr, Element arr a, Monoid b)
  => RingBuffer arr a
  -> (a -> IO b)
  -> IO b
foldMap rb action = withRing rb $ \ba bs -> do
  n <- filledLength rb
  let go !ix !acc = if ix < n
        then do
          v <- Contiguous.read ba ix
          m <- action v
          go (ix + 1) (acc <> m)
        else
          pure (bs, acc)
  go 0 mempty
{-# inline foldMap #-}

toList :: (Contiguous arr, Element arr a)
  => RingBuffer arr a
  -> IO [a]
toList rb = do
  len <- filledLength rb
  mapM (unsafeLatest rb) [0..len-1]
