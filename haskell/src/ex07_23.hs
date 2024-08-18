-- exercise 7-23, tree diameter
module Main where

import Data.Function (on)
import Data.Foldable (maximumBy)

data Tree = Branch [Tree]
          | Leaf
          deriving (Show)

type Diameter = Int
type Depth = Int

-- Definition: the diameter of a tree is the sum of the depths of its two
--             deepest subtrees.
--
--             If a tree has a single subtree, its diameter is the maximum of
--             its depth and the diameter of its subtree.
--
--             The diameter of a leaf is 0.
--
-- From each node of the tree, return both its diameter and depth.

treeDiameter :: Tree -> (Diameter, Depth)
treeDiameter Leaf = (0, 1)
treeDiameter (Branch ts) = let (diameters, depths) = unzip $ fmap treeDiameter ts
                               maxTwoDepths = max2 depths
                               newDiameter = max (sum maxTwoDepths) (maximum diameters)
                            in (newDiameter, 1 + (head maxTwoDepths))

max2 :: [Int] -> [Int]
max2 []  = []
max2 [x] = [x]
max2 xs  = go xs (minBound, minBound)
  where go []     (m1, m2)             = [m1, m2]
        go (y:ys) (m1, m2) | y >= m1   = go ys (y, m1)
                           | y >= m2   = go ys (m1, y)
                           | otherwise = go ys (m1, m2)

tree1 :: Tree
tree1 =
    Branch [
        Branch [
            Branch [
                Leaf,
                Leaf,
                Branch [Leaf, Leaf]
            ]
        ],
        Branch [Leaf, Leaf],
        Branch [Branch [Leaf, Leaf]]
    ]

tree2 :: Tree
tree2 =
    Branch [
        Branch [
            Branch [
                Branch [Leaf, Leaf]
            ],
            Branch [Branch [Leaf]]
        ],
        Leaf
    ]

tree3 :: Tree
tree3 = Branch [Leaf, Leaf]

main :: IO ()
main = print "hi"
