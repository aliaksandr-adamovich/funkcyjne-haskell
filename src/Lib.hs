module Lib (isSorted) where

isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted _ [] = True
isSorted _ [_] = True
isSorted cmp (x:y:rest) = cmp x y && isSorted cmp (y:rest)