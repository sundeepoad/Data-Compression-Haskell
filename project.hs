import Data.List
import System.IO
import qualified Data.Map as M
import Data.Char  
import Control.Monad
import qualified Data.ByteString.Lazy as BL 


--Takes the input text file and produces a file containing bit codes of the words 
--Main function for encoding the Text

encode :: IO ()   
encode = do     
    contents <- readFile "test.txt" 
    writeFile "output.txt" (encodeString contents)



--Tried a bit of ByteString to convert encoded bits into Binary bits
{-
main = do
  contents <- BL.readFile "test.txt"
  BL.writeFile "encoded.txt" (encodeString contents)
-}



--------TREE DATA TYPE----------------
data Tree a = Leaf a Int | Node (Tree a) (Tree a) Int deriving (Show)




instance (Eq a) => Eq (Tree a) where
 (Leaf aa ac)     == (Leaf bb bc)  = (aa == bb) && (ac == bc)
 (Node a1 a2 ac) == (Node b1 b2 bc) = (a1 == b1) && (a2 == b2) && (ac == bc)
 _   == _  = False



instance (Eq a) => Ord (Tree a) where
 a <= b = (freq a) <= (freq b)


--Takes a tree and it returns the integer frequency value of the Node
freq :: Tree a -> Int
freq l@(Leaf _ w) = w
freq t@(Node _ _ w) = w




--Takes every element from from list of tuples and makes it a Leaf
-- (\(a, count) -> Leaf a count) is a Lambda
leaf_mapping :: [(a, Int)] -> [Tree a]
leaf_mapping tup = map (\(a, count) -> Leaf a count) tup 




--Generates minimum frequency
getMinfrequency :: Ord a => [a] -> (a, [a])
getMinfrequency lst = (minimum lst , delete (minimum lst) lst)
                 


--Takes two nodes and adds the frequency/weight
mergePair :: Tree a -> Tree a -> Tree a
mergePair x y = Node x y ((freq x) + (freq y))



--Helper function for making Huffman Tree
merge_Lowest :: (Eq a) => [Tree a] -> [Tree a]
merge_Lowest lst = (mergePair n1 n2):xs
                        where
                        (n1,x) = getMinfrequency lst
                        (n2, xs) = getMinfrequency x


--Builds initial tree
treeBuilder :: (Eq a) => [Tree a] -> Tree a
treeBuilder lst = case (length lst) of
                        0 -> error "Empty"
                        1 -> lst !! 0
                        _ -> treeBuilder $ merge_Lowest lst
  
  



--Assigns 0 value if left edge and 1 if right edge
serialize :: Tree Char -> String -> [(Char, String)]
serialize (Leaf a _) prefix = [(a, prefix)]
serialize (Node lft rgt _) prefix = (serialize lft $ prefix ++ "0") ++ (serialize rgt $ prefix ++ "1")




--This uses composition of funciton. First leaf map gets called and then tree is built after that it is serialized to 0 and 1 to assign bits
huffmanTree :: [(Char, Int)] -> [(Char, String)]
huffmanTree lst = serialize (treeBuilder (leaf_mapping lst)) ""



--Function to generate tuples of characters with their frequency
-- EG:  "Habib" =>   [('a',1),('b',2),('h',1),('i',1)]
generateFreq :: Ord a => [a] -> [(a, Int)]
generateFreq lst =(map (\x -> (head x, length x)) $ group (sort lst))



--Generates bits for each character.  
bitGenerator :: [Char] -> [(Char, String)]
bitGenerator lst =(huffmanTree (generateFreq lst))



--Generates corresponding bits for a String
-- Two inputs:  1)  huffman ==> output of bitGenerator            2) lst = Simple string 
correspondingBits :: Eq a => [(a, t)] -> [a] -> [t]
correspondingBits _ [] = []
correspondingBits huffman lst@(y:ys) = [snd x | x <- huffman, fst (x) == y ] ++ correspondingBits huffman ys



--Gives the bits for a string 
-- Main function that perform Encoding of Text
--------------ENCODES THE GIVEN STRING-----------------
encodeString :: [Char] -> [Char]
encodeString lst = concat (correspondingBits (bitGenerator lst) lst)
		



---------Decoding the encoded bits--------------


-- Performs the decoding of Given Bits
-- Arguments: 1) bits => Output of encodeString
--			      2) codes => Output of bitGenerator
decodeBits :: Eq a => [a] -> [(t, [a])] -> Maybe [t]            
decodeBits bits codes = proceed bits where

  -- if the code matchCase the input, return the corresponding 
  -- Char value along with the rest of the input
  matchCase (val, xs) ys = moveForward xs ys where
    moveForward (x:xs) (y:ys) | x == y = moveForward xs ys
    moveForward []     ys              = Just (val, ys)
    moveForward _      _               = Nothing

  -- msum takes the first Just from a list of Maybe-s, 

  --proceed :: [Bit] -> Maybe String
  proceed [] = Just[]
  proceed xs = do
    (val, xs) <- msum (map (`matchCase` xs) codes)
    (val:) `fmap` proceed xs





decodeTest :: [Char] -> Maybe [Char]
decodeTest lst = decodeBits (encodeString lst) (bitGenerator lst)




-----Decode shows error when we try to apply it in IO. The error is because of return Type which we couldn't recitify on time. 

{-
decode = do     
  	contents <- readFile "output.txt"     -- Read contents from file that contains Bit representation  of given Text 
    sample <- readFile "test.txt"         -- Read The original text file 
    writeFile "decoded.txt" (decodeBits (contents) (bitGenerator sample))   -- Write an Output file that contains Decoded version of Encoded Text 

-}
