{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import RingBuffers.Lifted as R

main :: IO ()
main = do
  quickCheck (testAppend @Int)

testAppend :: forall a. (Eq a, Show a)
  => Positive Int
  -> [a]
  -> Property
testAppend (Positive cap) xs = monadicIO $ do
  xs' <- liftIO $ do
    rb <- R.new @a cap
    forM_ (reverse xs) $ \x -> R.append x rb
    R.toList rb
  pure $ counterexample (show xs') $ xs' == take cap xs
