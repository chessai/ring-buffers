{-# language TypeApplications #-}

module Main (main) where

import Data.List
import RingBuffers.Lifted as R
import Test.HUnit
import qualified GHC.Exts as Exts

main :: IO Counts
main = do
  runTestTT $ test
    [ testExtend
    ]

testExtend :: Test
testExtend = TestCase $ do
  let len = 40
  rb <- R.new @Int len

  let firstArr = Exts.fromList [0..4]
  let secondArr = Exts.fromList [0..50]
  R.extend firstArr rb
  R.extend secondArr rb

  checkLength rb len

  let expectedItems = []
        ++ Exts.toList firstArr
        ++ take (len - length firstArr) (Exts.toList secondArr)

  items <- R.toList rb
  putStrLn ""
  print items
  --if expectedItems == items
  --  then pure ()
  --  else pure () --fail "lol"
  --checkItems rb expectedItems

--  withItems rb $ \items -> do

checkLength :: RingBuffer a -> Int -> Assertion
checkLength rb expected = do
  len <- R.filledLength rb
  let msg = "Expected length " ++ show expected
  assertEqual msg expected len

checkItems :: (Eq a, Show a) => RingBuffer a -> [a] -> Assertion
checkItems rb expected = do
  items <- R.toList rb
  print items
  let msg = "Expected items " ++ show expected
  assertEqual msg expected [] --items
