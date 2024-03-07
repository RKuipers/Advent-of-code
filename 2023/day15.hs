--module Day15 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )
import Data.Char ( ord )
import Data.Map as M ( insert, keys, elems, empty, (!), adjust, Map ) 

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

data Node = Node {label :: String,
                  focal :: Maybe Int}
    deriving (Eq, Show)

type LineType = [Int]
type InputType = [LineType]
type ResultType = Int

type LineType2 = Node
type InputType2 = [LineType2]

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (sum $ map processLine1 parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseLines1 :: [String] -> InputType
parseLines1 lines = map (map ord) $ splitWhen (==',') $ concat lines

parseNode :: String -> Node
parseNode s | elem '=' s = let [l, f] = splitWhen (=='=') s in
                           Node l (Just $ read f)
            | elem '-' s = Node (filter (not.(=='-')) s) Nothing

parseLines2 :: [String] -> InputType2
parseLines2 lines = map parseNode $ splitWhen (==',') $ concat lines

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

processLine1 :: LineType -> ResultType
processLine1 = foldl (\t x -> 17 * (t + x) `mod` 256) 0

hash :: String -> Int
hash s = processLine1 $ map ord s

addNode :: Map Int [Node] -> Node -> Map Int [Node]
addNode m (Node l Nothing)    = adjust (filter (\(Node l' _) -> not $ l == l')) (hash l) m
addNode m n@(Node l (Just f)) | elem h (keys m) && inNodes = adjust (map (\n'@(Node l' _) -> if l == l' then n else n')) h m
                              | elem h (keys m)            = adjust (\ns -> ns ++ [n]) h m
                              | otherwise                  = insert h [n] m
    where h = hash l
          inNodes = elem l $ map (\(Node l' _) -> l') (m ! h)

calcFocus :: Int -> [Node] -> Int
calcFocus _  []                     = 0
calcFocus sn ((Node l (Just f)):ns) = (1 + hash l) * sn * f + calcFocus (sn+1) ns

processInput2 :: InputType2 -> ResultType
processInput2 ns = sum $ map (calcFocus 1) $ elems $ foldl addNode empty ns
--processInput2 ns = foldl addNode empty ns