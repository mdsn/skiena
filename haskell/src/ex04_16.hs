-- exercise 4-16, determine the fewest number of segments whose union covers
-- the interval (0, m).
module Main where

import Data.Function (on)
import Data.List (sortBy, maximumBy)

type Segment = (Int, Int)

solve :: [Segment] -> Int -> Maybe Int
solve segments m = fold 0 (0, 0) $ sortBy (compare `on` fst) segments
  where
    fold :: Int -> Segment -> [Segment] -> Maybe Int
    fold c current ss
      | current `contains` m = Just c
      | otherwise = case span (intersect current) ss of
                        ([], _)   -> Nothing
                        (xs, ss') ->
                            let next = maximumBy (compare `on` snd) xs
                             in fold (c+1) next ss'

    intersect a b = not (snd b < fst a) && not (snd a < fst b)
    contains a x  = intersect a (x,x)

main :: IO ()
main = do
    --        a         b         c         d         e        f
    let s = [(-1, 10), (85, 95), (12, 25), (40, 90), (8, 40), (80, 120)]
    print $ solve s 100
    -- Just 4 (a e d f)
