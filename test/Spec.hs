-- TODO Add tests for <<<, >>>, <<$ and $>>
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, (==>), withMaxSuccess)

import qualified Overture

succeed :: Assertion
succeed = assertBool "" True

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ testCase "takeEvery 2 from integers is same as filter even" $
      let integers = [0..100]
      in Overture.takeEvery 2 integers @?= filter even integers

  , testCase "takeEveryLast 2 from integers is same as filter odd" $
      let integers = [0..100]
      in Overture.takeEveryLast 2 integers @?= filter odd integers

  , testCase "withIndex indexes from 0" $
      let integers = [0..100]
      in assertBool "not all pairs match" $
           and $ (uncurry (==)) <$> (Overture.withIndex integers)
  , testProperty "product' and product are equivalent" $
      \(numberList :: [Integer]) ->
        product numberList == Overture.product' numberList
  , testProperty "sum' and sum are equivalent" $
      \(nums :: [Integer]) ->
        sum nums == Overture.sum' nums
  , testProperty "chunksOf empty list produces empty list" $
      \n ->
        Overture.chunksOf n ([] :: [Integer]) == [[]]
  , testProperty "chunksOf splits list into chunks" $
      \n (nums :: [Integer]) ->
        n > 0 && not (null nums) ==>
          length (Overture.chunksOf n nums) == ceiling ((fromIntegral $ length nums) / (fromIntegral n))
  , testProperty "chunksOf leaves maximal chunk at end" $
      \n (nums :: [Integer]) ->
        n > 0 && not (null nums) ==>
          if length nums `mod` n == 0
          then length (last (Overture.chunksOf n nums)) == n
          else length (last (Overture.chunksOf n nums)) == length nums `mod` n
  , testProperty "chunksOf preserves original list" $
      \n (nums :: [Integer]) ->
        n > 0 ==>
          concat (Overture.chunksOf n nums) == nums
  , testProperty "replace replaces all instances" $
      \n x (y::Int) -> Overture.replace x y (replicate n x) == replicate n y
  , testProperty "replace does not replace unduly" $
      \n z x (y::Int) -> x /= y ==>
        Overture.replace x z (replicate n y) == replicate n y
  , testCase "replaceWhere even" $
      (Overture.replaceWhere even (subtract 1) [1..100]) @?= (Overture.replicateElems 2 [1,3..100])
  ]
