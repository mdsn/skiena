module Main where

import Data.List (groupBy, sortBy)

-- A coordinate is just an integer point, since these are 1-dimensional
-- intervals.
type Coord = Int
-- Input intervals are represented as the left and right endpoints (inclusive).
data Interval = Interval Coord Coord deriving Show
-- Endpoints are categorized as opening an interval, closing an interval, or
-- both at once (e.g. [1,1]).
data End = Open | Close | Singular deriving Show
-- After manipulating the input intervals and sorting their coordinates, they
-- are collected into groups that meet at the same coordinate.
data Group = Group Coord [End] deriving Show
-- The running maximum is the number of intervals that meet at the coordinate
-- of a specific group, along with the coordinate itself.
data Maximum = Maximum Int Coord deriving Show

-- The example from the book. All intervals' endpoints are distinct.
-- There are two intervals where a point would lie in three of the original
-- intervals, [20,40] and [50,60]. Any one of the endpoints of these intervals
-- would do as an answer.
example :: [Interval]
example = [Interval 10 40, Interval 20 60, Interval 50 90, Interval 15 70]

-- A trickier example. Interval endpoints coincide, and there is a single-point
-- interval. There is one point where a point would lie in four of the original
-- intervals, which is naturally p = 30. This is the answer we expect.
example2 :: [Interval]
example2 = [Interval 10 40, Interval 15 30, Interval 30 50, Interval 30 30]

-- O(n). Combine the list of input intervals into a single list of categorized
-- coordinates.
combine :: [Interval] -> [(End, Coord)]
combine = foldr f []
  where
    f (Interval start end) ys
      | start /= end = (Open, start) : (Close, end) : ys
      | otherwise    = (Singular, start) : ys

-- O(n log n), I think Haskell uses merge sort for lists. Sort the combined
-- list of categorized coordinates by coordinate. This will put all the
-- interval endpoints together.
sortPoints :: [(End, Coord)] -> [(End, Coord)]
sortPoints = sortBy $ \(_, x) (_, y) -> compare x y

-- (Theoretically O(n)). Group the categorized coordinates by coordinate.
-- No idea of the complexity of the implementation of groupBy in Haskell but
-- since we have sorted the points by the component by which we now group them,
-- this is theoretically doable in O(n) as all points in a group will lie next
-- to each other.
groupPoints :: [(End, Coord)] -> [Group]
groupPoints xs = fmap makeGroup groups
  where
    groups = groupBy (\(_, x) (_, y) -> x == y) xs

    makeGroup :: [(End, Coord)] -> Group
    makeGroup (x:xs') = Group (snd x) $ fmap fst (x:xs')
    makeGroup [] = error "unreachable"

-- The central algorithm. Traverse the group of endpoints updating a counter of
-- ongoing intervals at each coordinate where an endpoint lies. These are the
-- only places where said counter can change, and since these are closed
-- intervals, they are as good as any other point to determine the maximum
-- number of intersecting intervals.
--  This algorithm visits a group only once, and within each group, it visits
-- its grouped endpoints once each. Therefore it is O(n) time on both the
-- number of groups and of endpoints in the groups.
determinePoint :: [Group] -> Maximum
determinePoint groups = findMaximum groups 0 Nothing
  where
    findMaximum :: [Group] -> Int -> Maybe Maximum -> Maximum
    findMaximum ((Group coord points):xs) counter m =
        let (opening, closing, singular) = count points
            -- The updated counter is determined only by the number of
            -- intervals that open and close at this coordinate.
            counter' = counter - closing + opening
            -- The number of intersecting intervals at this coordinate is
            -- whatever number of intervals were already running so far
            -- (counter), plus any new intervals that open, plus any 1-point
            -- intervals. Closing intervals have no effect because they were
            -- already counted when being open, so they are already in counter.
            intersecting = counter + opening + singular
            -- Update the running maximum+coordinate.
            m' = case m of
                Nothing -> Maximum intersecting coord
                Just (Maximum x c) | intersecting > x -> Maximum intersecting coord
                                   | otherwise        -> Maximum x c
         in findMaximum xs counter' (Just m')
    findMaximum [] _ (Just m) = m
    findMaximum [] _ Nothing = error "unreachable"

    -- Given the list of endpoints meeting at some coordinate, count how many
    -- are opening and closing intervals, and how many are 1-point intervals.
    count :: [End] -> (Int, Int, Int)
    count = foldr f (0, 0, 0)
      where f Open     (o, c, s) = (o+1, c, s)
            f Close    (o, c, s) = (o, c+1, s)
            f Singular (o, c, s) = (o, c, s+1)

main :: IO ()
main = do
    print $ determinePoint . groupPoints . sortPoints . combine $ example2
