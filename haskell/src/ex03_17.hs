-- exercise 3-17
-- give an O(n) algorithm that determines if a given binary tree is
-- height-balanced. a tree is height-balanced if the difference between the
-- left and right subtrees of every node is at most 1.
--
-- a recursive solution comes naturally from the definition of height balance,
-- although the recursive aspect of the definition is implicit in the book (ex
-- 3-15). A tree is height balanced if its two subtrees are height balanced and
-- their heights differ by at most 1. The base case for this, Nil nodes in our
-- tree definition, is that these nodes are balanced and have height 0. The
-- recursive call into isBalanced returns both the boolean result we look for
-- as well as the height of the checked subtree--which is good because we want
-- this height to both determine if the current node is balanced as well as to
-- compute the height to return.
module Main where

data Tree = Node Tree Tree
          | Nil

--      *       1
--    /   \
--   X     *    2   The left subtree of X is height 1, while the right
--  / \   / \       subtree of X is height 3. This tree is not
-- *   * *   *  3   height-balanced.
--    / \   /
--   *   * *    4
--    \
--     *        5
unbalancedTree :: Tree
unbalancedTree =
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

--     *      1
--    / \
--   *   *    2
--  /   / \
-- *   *   *  3
balancedTree :: Tree
balancedTree =
    Node
        (Node
            (Node Nil Nil)
            Nil)
        (Node
            (Node Nil Nil)
            (Node Nil Nil))

isBalanced :: Tree -> (Bool, Int)
isBalanced Nil = (True, 0)
isBalanced (Node left right) = (balanced, height)
    where (lb, lh) = isBalanced left
          (rb, rh) = isBalanced right
          height = 1 + max lh rh
          balanced = lb && rb && abs (lh - rh) <= 1

main :: IO ()
main = do
    print $ isBalanced unbalancedTree
    print $ isBalanced balancedTree
