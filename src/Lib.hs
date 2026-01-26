module Lib (
    isSorted,
    toTwoArgs,
    sumRows3,
    setHead,
    appendAt,
    squarePairs,
    addOrSub,
    concatThree,
    sumOrNil,
    stateInt,
    stateDouble,
    statePairs,
    stateDoubleMap,
    stateDoubleFlatMap,
    ) where

import Data.Int (Int64)
import Control.Monad (guard)

isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted _ [] = True
isSorted _ [_] = True
isSorted cmp (x:y:rest) = cmp x y && isSorted cmp (y:rest)


toTwoArgs :: (a -> b -> c -> d) -> a -> (b, c) -> d
toTwoArgs f a (b, c) = f a b c


sumRows3 :: Num n => [n] -> [n] -> [n] -> [n]
sumRows3 xs ys zs =
  zipWith (toTwoArgs (\x y z -> x + y + z)) xs (zip ys zs)

setHead :: a -> [a] -> [a]
setHead x xs = x : xs

appendAt :: Int -> a -> [a] -> [a]
appendAt idx v xs | idx <= 0 = v : xs
appendAt _ v [] = [v]
appendAt idx v (x:rest) = x : appendAt (idx - 1) v rest


squarePairs :: Num n => [n] -> [n] -> [n]
squarePairs xs ys =
  map (\(x, y) -> (x + y) * (x + y)) (zip xs ys)

addOrSub :: Bool -> Int -> Int -> Int
addOrSub isAdd a b =
  head $ map op [a]
  where
    op = if isAdd then (+ b) else subtract b


concatThree :: [a] -> [a] -> [a] -> [a]
concatThree a b c =
 mconcat [a, b, c]

sumOrNil :: [Int] -> Maybe Int
sumOrNil xs = do
  guard (not (null xs))
  return (sum xs)

  ----------------------------------------------------------------------

stateInt :: Int -> Int
stateInt s = abs s `mod` (maxBound :: Int)

stateDouble :: Int -> Double
stateDouble s =
  fromIntegral (abs s `mod` 1000000) / 1000000

statePairs :: Int -> ((Int, Double), (Double, Int), (Double, Double, Double))
statePairs s =
  let i = stateInt s
      d = stateDouble s
  in ((i, d), (d, i), (d, d, d))


stateDoubleMap :: Int -> Double
stateDoubleMap s =
  head $ map stateDouble [s]


stateDoubleFlatMap :: Int -> Double
stateDoubleFlatMap s =
  head $ concatMap (\x -> [stateDouble x]) [s]
