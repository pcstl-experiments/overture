module Overture
    ( takeEvery
    , takeEveryLast
    , withIndex
    , replace
    , (.)
    , (&)
    , ($)
    , (<$>)
    , (<&>)
    , (<<<)
    , (>>>)
    , ($>>>)
    , (<<<$)
    ) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Arrow ((<<<), (>>>))

(<<<$) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <<<$ g = fmap f . g

($>>>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f $>>> g = fmap g . f

replace :: Eq a => a -> a -> [a] -> [a]
replace el _   []     = []
replace el rep (x:xs)
  | x == el   = rep : (replace el rep xs)
  | otherwise = x : (replace el rep xs)

takeEvery :: Int -> [a] -> [a]
takeEvery n []     = []
takeEvery n (x:xs) = x : (takeEvery n (drop (n-1) xs))
  
takeEveryLast :: Int -> [a] -> [a]
takeEveryLast n list =
  takeEvery n (drop (n-1) list)

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]
