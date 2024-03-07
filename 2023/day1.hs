import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Maybe ( fromJust )

main :: IO ()
main = do    
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines = map parseLine $ lines
    putStrLn $ "Part 1: " ++ show (sum parsedLines)
    let parsed2Lines = map parseWordLine $ lines
    putStrLn $ "Part 2: " ++ show (sum parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

getNumberFromLine :: String -> Int
getNumberFromLine [] = 0
getNumberFromLine xs = read [head xs]

parseLine :: String -> Int
parseLine line = do
    let numbers = filter (\x -> x >= '0' && x <= '9') line
    let reversed = reverse numbers
    10 * getNumberFromLine numbers + getNumberFromLine reversed

parseWordLine :: String -> Int
parseWordLine line = do
    let digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
        match = lrcrawl line digitWords
        leftDigit = if length match == 1 then match else show (1 + fromJust (elemIndex match digitWords))
        match2 = rlcrawl line digitWords
        rightDigit = if length match2 == 1 then match2 else show (1 + fromJust (elemIndex match2 digitWords))
    read (leftDigit ++ rightDigit) :: Int

lrcrawl :: String -> [String] -> String
lrcrawl [] words = ""
lrcrawl line words = do
    let matches = filter (\x -> isPrefixOf x line) words
    if length matches > 0 then head matches else lrcrawl (tail line) words

rlcrawl :: String -> [String] -> String
rlcrawl [] words = ""
rlcrawl line words = do
    let matches = filter (\x -> isSuffixOf x line) words
    if length matches > 0 then head matches else rlcrawl (init line) words