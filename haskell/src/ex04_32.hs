-- exercise 4-32, wiggle sort
module Main where

import Data.Ord

wiggle :: [Int] -> [Int]
wiggle xs = go GT xs
  where
    go :: Ordering -> [Int] -> [Int]
    go _  (a:[])               = [a]
    go LT (a:b:xs) | a < b     = b : (go GT (a:xs))
                   | otherwise = a : (go GT (b:xs))
    go GT (a:b:xs) | a > b     = b : (go LT (a:xs))
                   | otherwise = a : (go LT (b:xs))

main :: IO ()
main = do
    print $ wiggle [3,1,4,2,6,5]    -- [1 < 4 > 2 < 6 > 3 < 5]
    print $ wiggle [1,2,3,4,5,6]    -- [1 < 3 > 2 < 5 > 4 < 6]
    print $ wiggle [6,5,4,3,2,1]    -- [5 < 6 > 3 < 4 > 1 < 2]
