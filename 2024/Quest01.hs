import Data.List.Split (chunksOf)
import Common (count)

main = do
    readFile "inputs/q01p1.txt" >>= print . p1
    readFile "inputs/q01p2.txt" >>= print . p23 2
    readFile "inputs/q01p3.txt" >>= print . p23 3

p1 :: String -> Int
p1 = sum . map potions

p23 :: Int -> String -> Int
p23 n s = p1 s + sum (map groups $ chunksOf n s)

groups :: String -> Int
groups g = case count (/= 'x') g of
    2 -> 2
    3 -> 6
    _ -> 0

potions :: Char -> Int
potions x = case x of
    'A' -> 0
    'x' -> 0
    'B' -> 1
    'C' -> 3
    'D' -> 5
