import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

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
addNats nat Zero = nat
addNats (Succ nat) nat2 = addNats nat (Succ nat2)

multiplyNats :: Nat -> Nat -> Nat
multiplyNats Zero nat = Zero
multiplyNats nat Zero = Zero
multiplyNats (Succ nat) nat2 = addNats nat2 (multiplyNats nat nat2)

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
    | x > val   = (Node val left (treeInsert right x))
    | otherwise = (Node val left right)


morseMap = Map.fromList[("a", ".-"),  
                        ("b", "-..."),
                        ("c", "-.-."),
                        ("d", "-.."),
                        ("e", "."),
                        ("f", "..-."),
                        ("g", "--."),
                        ("h", "...."),
                        ("i", ".."),
                        ("j", ".---"),
                        ("k", "-.-"),
                        ("l", ".-.."),
                        ("m", "--"),
                        ("n", "-."),
                        ("o", "---"),
                        ("p", ".--."),
                        ("q", "--.-"),
                        ("r", ".-."),
                        ("s", "..."),
                        ("t", "-"),
                        ("u", "..-"),
                        ("v", "...-"),
                        ("w", ".--"),
                        ("x", "-..-"),
                        ("y", "-.--"),
                        ("z", "--..")]

-- Simple encoding of characters
encodeCharacter :: Char -> String
encodeCharacter c =
    let Just char = Map.lookup [c] morseMap
    in char

-- Encode character using filtering map (just for training purposes)
encodeCharacter' :: Char -> [Char]
encodeCharacter' c = value
    where map' = Map.filterWithKey (\k _ -> k == [c]) morseMap
          (key, value) = Map.findMin map'

-- Encode a string using standard ecnoding
encodeMorse :: String -> String
encodeMorse [] = ""
encodeMorse [c] = encodeCharacter c
encodeMorse (c:cs) = encodeCharacter c ++ encodeMorse cs

-- Utility function to convert list of strings to a string
-- Useful when mapping strings to morse code, since we map LETTERS in message to STRINGS in morse code
convertToString :: [String] -> String
convertToString [] = ""
convertToString [s] = s
convertToString (s:ss) = s ++ convertToString ss

-- Better encode, using mapping and my ultity function
encodeMorse' :: [Char] -> [Char]
encodeMorse' s =  convertToString $ map encodeCharacter' s

decodeCharacter :: String -> Maybe [Char]
decodeCharacter s
    | Map.null map' = Nothing
    | otherwise = Just key
    where map' = Map.filterWithKey (\_ v -> v == s) morseMap
          (key, value) = Map.findMin map'

decodeMorse :: String -> [String]
decodeMorse [] = [""]
decodeMorse [x]
-- decodeMorse [x] = case of decoded
--     (Just val) -> [val]
--     Nothing -> [""]
    | Maybe.isJust decoded = [Maybe.fromJust decoded]
    | otherwise = [""]
    where decoded = decodeCharacter ([x])
decodeMorse (x:y:xs)
    | Maybe.isJust decoded = [Maybe.fromJust decoded]
    | otherwise = (decodeMorse [x]) ++ (decodeMorse ([y]++xs)) ++ (decodeMorse ([x]++[y]) ++ decodeMorse xs)
    where decoded = decodeCharacter ([x] ++ [y] ++ xs)

decodeMorse' :: String -> String -> [String]
decodeMorse' x []
    | Maybe.isJust decoded = [Maybe.fromJust decoded]
    | otherwise = [""]
    where decoded = decodeCharacter (x)
decodeMorse' str (x:xs) 
    | Maybe.isJust decoded = (([ a++b | a<- [Maybe.fromJust decoded], b <- (decodeMorse' [x] xs) ])) ++ (decodeMorse' (str++[x]) xs)
    | otherwise = decodeMorse' (str++[x]) xs
    where decoded = decodeCharacter str

main :: IO ()
main = do print $ addNats (Succ (Succ Zero)) (Succ (Succ Zero))
          print $ addNats (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))
          print $ multiplyNats (Succ (Succ Zero)) (Succ (Succ Zero))
          print $ multiplyNats (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))
          print $ nat2int (Succ (Succ (Succ Zero)))
          print $ int2nat 5
          print $ treeInsert (Node 10 (Node 5 Leaf Leaf) (Node 15 Leaf Leaf)) 7
          print $ treeInsert (Node 10 (Node 5 Leaf Leaf) (Node 15 Leaf Leaf)) 17
          print $ encodeMorse "doodoo"
          print $ encodeMorse' "doodoo"
          print $ decodeCharacter "------"
          print $ decodeCharacter "..."
          --print $ decodeMorse ".--...-.-."
          print $ decodeMorse' "" ".-..-"
-- 1 Succ Zero
-- 2 Succ (Succ Zero)
-- 3 Succ (Succ (Succ Zero))