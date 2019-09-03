module Ex1 where

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos 
moves []     pos = pos
moves (x:xs) pos = moves xs (move x pos)

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord) 

addNats :: Nat -> Nat -> Nat
addNats Zero nat = nat
addNats (Succ nat) nat2 = addNats nat (Succ nat2)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ nat) = 1 + nat2int nat

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = (Succ (int2nat (x-1)))

-- All leaves in left/right subtree are less/greater than node value
data Tree = Leaf | Node Int Tree Tree
    deriving (Eq, Show, Read, Ord)

treeInsert :: Tree -> Int -> Tree
treeInsert Leaf x = Node x Leaf Leaf
treeInsert (Node val left right) x
    | x < val   = (Node val (treeInsert left x) right) 
    | otherwise = (Node val left (treeInsert right x))


-- 1 Succ Zero
-- 2 Succ (Succ Zero)
-- 3 Succ (Succ (Succ Zero))