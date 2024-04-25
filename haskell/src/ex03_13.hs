-- exercise 3-13, find and swap the wrongly positioned nodes
-- in a binary tree in O(n) time.
module Main where

data Tree a = Node a (Tree a) (Tree a)
            | Nil
    deriving Show

--      8
--    /   \
--   2     12
--  / \   / \
-- 1   6 9   15
--    / \   /
--  *7  *3 14   Wrong!
--        \
--         4
tree :: Tree Int
tree =
    Node 8
        (Node 2
            (Node 1 Nil Nil)
            (Node 6
                (Node 7 Nil Nil)
                (Node 3
                    Nil
                    (Node 4 Nil Nil))))
        (Node 12
            (Node 9 Nil Nil)
            (Node 15
                (Node 14 Nil Nil)
                Nil))

-- The (big) assumption in this solution is that the elements
-- swapped are siblings. This naturally won't work if elements
-- are swapped across levels.
fixTree :: Ord a => Tree a -> Tree a
fixTree Nil = Nil
fixTree (Node x l@(Node xl _ _) r@(Node xr _ _))
    | xl > xr   = Node x r l
    | otherwise = Node x (fixTree l) (fixTree r)
fixTree t = t


main :: IO ()
main = do
    print "Before"
    print tree
    print "After"
    print $ fixTree tree
