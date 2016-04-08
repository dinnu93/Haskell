-- Folds & Monoids

import qualified Data.Monoid as M

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

-- treeSize : No.of nodes in a Tree.
treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node lTree node rTree) = 1 + (treeSize lTree) + (treeSize rTree)

-- treeSum : Sum of all the nodes in a tree
treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node lTree node rTree) = node + (treeSum lTree) + (treeSum rTree)

-- treeDepth : Depth of a Tree
treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node lTree _ rTree) = 1 + max (treeDepth lTree) (treeDepth rTree)

-- flatten : Flattens a Tree into List
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node lTree node rTree) = (flatten lTree) ++ [node] ++ (flatten rTree)

-- Genralizing the above three functions we can implement a foldTree function
-- similar to foldl & foldr

-- foldTree :: Folds a Tree
foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree _ identity Empty = identity
foldTree f identity tree@(Node lTree node rTree) = f (foldTree f identity lTree) node (foldTree f identity rTree)

x = Node (Node Empty 5 Empty) 4 (Node Empty 6 Empty)

-- Monoids

