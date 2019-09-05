module Warmup where

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

add :: Nat -> Nat -> Nat
add Zero nat = nat
add nat Zero = nat
add (Succ nat) nat2 = add nat (Succ nat2)

mult :: Nat -> Nat -> Nat
mult Zero nat = Zero
mult nat Zero = Zero
mult (Succ nat) nat2 = add nat2 (mult nat nat2)

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ nat) = 1 + nat2int nat

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = (Succ (int2nat (x-1)))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Tree -> Int -> Tree
insert Leaf x = Node x Leaf Leaf
insert (Node val left right) x
    | x < val   = (Node val (insert left x) right) 
    | x > val   = (Node val left (insert right x))
    | otherwise = (Node val left right)