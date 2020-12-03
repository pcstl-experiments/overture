module Overture
    ( takeEvery
    , takeEveryLast
    , withIndex
    , (&)
    , ($)
    , (<$>)
    , (<&>)
    , (<<<)
    , (.)
    ) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((<<<))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

takeEvery :: Int -> [a] -> [a]
takeEvery n []     = []
takeEvery n (x:xs) = x : (takeEvery n (drop (n-1) xs))
  
takeEveryLast :: Int -> [a] -> [a]
takeEveryLast n list =
  takeEvery n (drop (n-1) list)

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]
