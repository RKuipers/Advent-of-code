--module Day8 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )
import Data.Map ( empty, insert, lookup, keys, Map )
import Data.Char ( isAlphaNum )

import Utils ( splitWhen )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type InputType = ([Bool], Map String (String, String))
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    --let parsedLines = map parseLine1 $ lines
    let parsedLines = parseLines1 lines
    --putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    putStrLn $ "Part 2: " ++ show (processInput2 parsedLines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseLines1 :: [String] -> InputType
parseLines1 (l:_:ls) = (cycle $ map (=='L') l, go ls)
    where go []        = empty
          go (line:ls) = let (s:l:r) = splitWhen (==' ') (filter (\c -> c == ' ' || isAlphaNum c) line) in
                         insert s (l, head r) $ go ls

--parseLine1 :: String -> LineType
--parseLine1 line = undefined

parseLines2 :: [String] -> InputType
parseLines2 line = undefined

--parseLine2 :: String -> LineType
--parseLine2 = parseLine1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

toNext :: Map String (String, String) -> Bool -> String -> String
toNext nodes dir curr = let Just next = Data.Map.lookup curr nodes in
                        if dir then fst next else snd next

processInput1 :: InputType -> ResultType
processInput1 (dirs, nodes) = go dirs "AAA"
    where go (d:ds) "ZZZ" = 0
          go (d:ds) curr  = 1 + go ds (toNext nodes d curr)

processInput2 :: InputType -> ResultType
processInput2 (dirs, nodes) = foldl lcm 1 $ map (go dirs) $ filter (\k -> last k =='A') $ keys nodes
    where go (d:ds) curr | last curr =='Z' = 0
                         | otherwise       = 1 + go ds (toNext nodes d curr)