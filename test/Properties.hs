{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (Last(..))
import GHC.Exts (fromList)
import Prelude hiding (foldMap)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.List as List
import qualified RingBuffers.Lifted as R

main :: IO ()
main = do
  quickCheck $ append @Int
  quickCheck $ newIsEmpty
  quickCheck $ latest @Int
  quickCheck $ foldMap @Int

append :: forall a. (Eq a, Show a)
  => Positive Int
  -> [a]
  -> Property
append (Positive cap) xs = monadicIO $ do
  xs' <- liftIO $ do
    rb <- R.new @a cap
    forM_ (reverse xs) $ \x -> R.append x rb
    R.toList rb
  pure $ counterexample (show xs') $ xs' == take cap xs

newIsEmpty :: ()
  => Positive Int
  -> Property
newIsEmpty (Positive cap) = monadicIO $ do
  rb <- liftIO $ R.new @Int cap
  rbCap <- liftIO $ R.capacity rb
  rbLen <- liftIO $ R.filledLength rb
  pure $ rbCap == cap && rbLen == 0

latest :: forall a. (Eq a, Show a)
  => Positive Int
  -> [a]
  -> Property
latest (Positive cap) xs = monadicIO $ do
  rb <- liftIO $ R.new @a cap
  let go = \case
        [] -> do
          pure ()
        (a : as) -> do
          liftIO $ R.append a rb
          l <- liftIO $ R.latest rb 0
          assert (Just a == l)
          go as
  go xs

foldMap :: forall a. (Ord a, Show a)
  => Positive Int
  -> [a]
  -> Property
foldMap (Positive cap) xs = monadicIO $ do
  rb <- liftIO $ R.new @a cap
  liftIO $ R.extend (fromList xs) rb
  m <- liftIO $ R.foldMap rb $ \x -> pure [x]
  pure $ counterexample (show m) $ all (`elem` xs) m
