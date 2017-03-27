module SearchTree(fromList, toList,
               find, insert, delete) where

data Tree a = Node a (Tree a) (Tree a) Int | Leaf deriving (Show, Eq)

height :: Tree a -> Int
height Leaf = 0
height (Node _ _ _ h) = h

left :: Tree a -> Tree a
left Leaf = Leaf
left (Node _ l _ _) = l

right :: Tree a -> Tree a
right Leaf = Leaf
right (Node _ _ r _) = r

hNode :: a -> Tree a -> Tree a -> Tree a
hNode v l r = Node v l r $ 1 + max (height l) (height r)

bNode :: a -> Tree a -> Tree a -> Tree a
bNode x l r = balance $ hNode x l r

xNode :: a -> Tree a -> Tree a -> Tree a
xNode x l r = Node x l r (-1)

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance x@(Node nVal nLeft nRight _) =
    case diff x of
    -2 -> let r = right x
              newR = if (diff r) > 0 then rotateRight r else r
          in rotateLeft $ xNode nVal nLeft newR
    2  -> let l = left x
              newL = if (diff l) < 0 then rotateLeft l else l
          in rotateRight $ xNode nVal newL nRight
    otherwise -> x

diff :: Tree a -> Int
diff Leaf = 0
diff (Node _ l r _) = (height l) - (height r)

rotateRight :: Tree a -> Tree a
rotateRight Leaf = Leaf
rotateRight (Node nVal (Node lVal lLeft lRight _) r _) = hNode lVal lLeft (hNode nVal lRight r)

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf = Leaf
rotateLeft (Node nVal l (Node rVal rLeft rRight _) _) = hNode rVal (hNode nVal l rLeft) rRight

-- find
findLeaf :: Ord a => a -> Tree a -> Tree a
findLeaf _ Leaf = Leaf
findLeaf x z@(Node n l r _) | x == n    = z
                            | x < n     = findLeaf x l
                            | otherwise = findLeaf x r

find :: Ord a => a -> Tree a -> Bool
find _ Leaf = False
find x (Node n l r _) | x == n    = True
                      | x < n     = find x l
                      | otherwise = find x r

leftmost :: Ord a => Tree a -> Tree a
leftmost Leaf = Leaf
leftmost n@(Node _ Leaf _ _) = n
leftmost (Node _ l _ _) = leftmost l

-- insert
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf 1
insert x (Node n l r _) | x < n     = bNode n (insert x l) r
                        | otherwise = bNode n l (insert x r)

-- from / to list
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

toList :: Ord a => Tree a -> [a]
toList Leaf = []
toList (Node n l r _) = (toList l) ++ [n] ++ (toList r)

-- delete
delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node z l r _) | x < z     = bNode z (delete x l) r
                        | x > z     = bNode z l (delete x r)
                        | otherwise = case leftmost r of Node q _ _ _ -> bNode q l (delete q r)
                                                         Leaf -> l

printTree :: Show a => Tree a -> IO ()
printTree = mapM_ putStrLn . treeIndent
  where
    treeIndent Leaf           = ["-- /-"]
    treeIndent (Node k lb rb _) =
      ["--" ++ show k] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent rb
        ls     = treeIndent lb