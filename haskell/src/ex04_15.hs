module Main where

import Data.List (groupBy, sortBy)

type Coord = Int
type Id = Int

data Interval = Interval Coord Coord deriving Show
data End = Open | Close | Singular deriving Show
-- data Point = Point End deriving Show
data Group = Group Coord [End] deriving Show
data Maximum = Maximum Int Coord deriving Show

-- The example from the book. All intervals' endpoints are distinct.
-- group . sort . combine =
-- [[Point 1 Open 10],
--  [Point 4 Open 15],
--  [Point 2 Open 20],
--  [Point 1 Close 40],
--  [Point 3 Open 50],
--  [Point 2 Close 60],
--  [Point 4 Close 70],
--  [Point 3 Close 90]]
-- There are two intervals where a point would lie in three of the original
-- intervals, [20,40] and [50,60]. Any one of the endpoints of these intervals
-- would do as an answer.
example :: [Interval]
example = [Interval 10 40, Interval 20 60, Interval 50 90, Interval 15 70]

-- A trickier example. Interval endpoints coincide, and there is a single-point
-- interval.
-- group . sort . combine =
-- [[Point 1 Open 10],
--  [Point 2 Open 15],
--  [Point 2 Close 30,Point 3 Open 30,Point 4 Singular 30],
--  [Point 1 Close 40],
--  [Point 3 Close 50]]
-- There is one point where a point would lie in four of the original
-- intervals, which is naturally p = 30. This is the answer we expect.
example2 :: [Interval]
example2 = [Interval 10 40, Interval 15 30, Interval 30 50, Interval 30 30]

-- O(n)
combine :: [Interval] -> [(End, Coord)]
combine intervals = go intervals
  where
    go :: [Interval] -> [(End, Coord)]
    go [] = []
    go ((Interval start end):xs)
      | start /= end = (Open, start) : (Close, end) : go xs
      | otherwise    = (Singular, start) : go xs

-- O(n log n), I think Haskell uses merge sort for lists.
sortPoints :: [(End, Coord)] -> [(End, Coord)]
sortPoints = sortBy $ \(_, x) (_, y) -> compare x y

-- No idea of the complexity of the implementation of this function in Haskell
-- but since we have sorted the points by the component by which we now group
-- them, this is theoretically doable in O(n) as all points in a group will lie
-- next to each other.
groupPoints :: [(End, Coord)] -> [Group]
groupPoints xs = fmap makeGroup groups
  where
    groups = groupBy (\(_, x) (_, y) -> x == y) xs

    makeGroup :: [(End, Coord)] -> Group
    makeGroup (x:xs') = Group (snd x) $ fmap fst (x:xs')
    makeGroup [] = error "unreachable"

determinePoint :: [Group] -> Maximum
determinePoint points = processGroups points 0 Nothing
  where
    processGroups :: [Group] -> Int -> Maybe Maximum -> Maximum
    processGroups (x:xs) counter Nothing =
        let (opening, closing, singular) = processGroup x
            counter' = counter - closing + opening
            intersecting = counter + opening + singular
            -- TODO update -1 with actual coord
         in processGroups xs counter' (Just $ Maximum intersecting (-1))
    processGroups (x@(Group coord points):xs) counter (Just (Maximum m p)) =
        let (opening, closing, singular) = processGroup x
            counter' = counter - closing + opening
            intersecting = counter + opening + singular
            (m', p') = if intersecting > m
                then (intersecting, coord)
                else (m, p)
         in processGroups xs counter' (Just $ Maximum m' p')
    processGroups [] _ (Just m) = m
    processGroups [] _ Nothing = error "unreachable"

    processGroup :: Group -> (Int, Int, Int)
    processGroup (Group _ xs) = go xs (0, 0, 0)
      where
        go :: [End] -> (Int, Int, Int) -> (Int, Int, Int)
        go (Open:xs') (o, c, s) = go xs' (o+1, c, s)
        go (Close:xs') (o, c, s) = go xs' (o, c+1, s)
        go (Singular:xs') (o, c, s) = go xs' (o, c, s+1)
        go [] (o, c, s) = (o, c, s)

main :: IO ()
main = do
    print $ determinePoint . groupPoints . sortPoints . combine $ example2
