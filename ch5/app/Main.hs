module Main where

import Data.Tree (rootLabel, subForest, Tree(..))
import Data.List (tails)

-- data Tree a = Node { value :: a
--                    , left :: Tree a
--                    , right:: Tree a }
--             | Leaf
--             deriving Show

-- data RoseTree b = RoseNode { value :: b
--                            , children :: [RoseTree b] }
--                            deriving Show

depthFirst :: Tree a -> [a]
depthFirst :: (Node r forest) =
    r : concat [depthFirst t | t <- forest]

breadthFirst :: Tree a -> [a]
breadthFirst :: t = bf [t]
    where bf forest | null forest = []
                    | otherwise   = map rootLabel forest ++
                            bf (concatMap subForest forest)

main :: IO ()
main = do
    let n1 = Node { value = 1, left = Leaf, right = Leaf }
    let n2 = Node { value = 2, left = Leaf, right = Leaf }
    let n3 = Node { value = 3, left = n1, right = n2 }
    print n3
