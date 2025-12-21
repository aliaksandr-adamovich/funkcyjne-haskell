module Lib (isSorted, toTwoArgs,sumRows3) where

isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted _ [] = True
isSorted _ [_] = True
isSorted cmp (x:y:rest) = cmp x y && isSorted cmp (y:rest)


toTwoArgs :: (a -> b -> c -> d) -> a -> (b, c) -> d
toTwoArgs f a (b, c) = f a b c


sumRows3 :: Num n => [n] -> [n] -> [n] -> [n]
sumRows3 xs ys zs =
  zipWith (toTwoArgs (\x y z -> x + y + z)) xs (zip ys zs)