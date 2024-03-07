--module Day3 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.List.Extra ( disjoint )
import Data.Maybe ( fromJust )
import Data.Char ( isDigit, digitToInt )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)
data Number = Number { value :: Int,
                      coords :: [Coord]}
    deriving (Show)

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let symbols1 = concat $ parseSymbols 0 lines
    let numbers = concat $ parseNumbers 0 lines
    putStrLn $ "Part 1: " ++ show (processInput1 symbols1 numbers)
    let symbols2 = concat $ parseSymbols2 0 lines
    --putStrLn $ "Symbols: " ++ show symbols2
    --putStrLn $ "Numbers: " ++ show numbers
    putStrLn $ "Part 2: " ++ show (processInput2 symbols1 numbers)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseSymbolLine :: (Char -> Bool) -> Int -> Int -> String -> [Coord]
parseSymbolLine p _ _ []     = []
parseSymbolLine p y x (z:zs) = if p z then parseSymbolLine p y (x+1) zs
                               else (x, y) : parseSymbolLine p y (x+1) zs

mkNumber :: Int -> Int -> Int -> [Int] -> Int -> [Coord] -> Number
mkNumber x y m []     v coords = Number v coords
mkNumber x y m (z:zs) v coords = mkNumber (x-1) y (m*10) zs (v+z*m) ((x, y):coords)

parseNumberLine :: Int -> Int -> String -> [Int] -> [Number]
parseNumberLine _ _ []     []     = []
parseNumberLine y x []     (a:as) = [mkNumber (x-1) y 1 (a:as) 0 []]
parseNumberLine y x (z:zs) []     = if isDigit z then parseNumberLine y (x+1) zs [digitToInt z]
                                    else parseNumberLine y (x+1) zs []
parseNumberLine y x (z:zs) (a:as) = if isDigit z then parseNumberLine y (x+1) zs (digitToInt z:a:as)
                                    else mkNumber (x-1) y 1 (a:as) 0 []:parseNumberLine y (x+1) zs []

parseSymbols :: Int -> [String] -> [[Coord]]
parseSymbols _ [] = []
parseSymbols y (x:xs) = (parseSymbolLine (\z -> isDigit z || z == '.') y 0 x) : parseSymbols (y+1) xs

parseNumbers :: Int -> [String] -> [[Number]]
parseNumbers _ [] = []
parseNumbers y (x:xs) = (parseNumberLine y 0 x []) : parseNumbers (y+1) xs

parseSymbols2 :: Int -> [String] -> [[Coord]]
parseSymbols2 _ [] = []
parseSymbols2 y (x:xs) = (parseSymbolLine (\z -> not $ z == '*') y 0 x) : parseSymbols2 (y+1) xs

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

getAdjacents :: Coord -> [Coord]
getAdjacents (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

processNumber :: [Coord] -> Number -> Int
processNumber coords (Number value coords') = if disjoint coords coords' then 0
                                              else value

processInput1 :: [Coord] -> [Number] -> Int
processInput1 c = foldr (\n x -> x + processNumber coords n) 0
   where coords = concatMap getAdjacents c

getAdjacentNumbers :: [Number] -> Coord -> [Number]
getAdjacentNumbers []                       _     = []
getAdjacentNumbers (n@(Number _ coords):xs) coord = if disjoint (getAdjacents coord) coords then getAdjacentNumbers xs coord
                                                    else n:getAdjacentNumbers xs coord

processGears :: [Number] -> Int
processGears [(Number v1 _), (Number v2 _)] = v1 * v2
processGears _                              = 0

processInput2 :: [Coord] -> [Number] -> Int
processInput2 cs ns = sum $ map processGears $ map (getAdjacentNumbers ns) cs