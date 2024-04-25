-- exercise 3-12, find the maximum depth of a binary tree in O(n) time.
module Main where

-- A binary tree without elements. Pure structure!
data Tree = Node Tree Tree
          | Nil

--      *       1
--    /   \
--   *     *    2
--  / \   / \
-- *   * *   *  3
--    / \   /
--   *   * *    4
--    \
--     *        5
tree :: Tree
tree =
    Node -- 1
        (Node -- 2
            (Node Nil Nil)
            (Node -- 3
                (Node -- 4
                    Nil
                    (Node Nil Nil)) -- 5
                (Node Nil Nil)))
        (Node
            (Node Nil Nil)
            (Node
                (Node Nil Nil)
                Nil))

maxDepth :: Tree -> Int
maxDepth Nil        = 0
maxDepth (Node l r) = 1 + max (maxDepth l) (maxDepth r)

main :: IO ()
main = print $ maxDepth tree
