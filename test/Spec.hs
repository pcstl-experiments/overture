import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty)

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
  ]
