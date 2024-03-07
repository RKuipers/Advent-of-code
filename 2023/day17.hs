--module Day17 where

import System.IO ()
import Data.List ( elemIndex, isPrefixOf, isSuffixOf )
import Data.Char ( digitToInt )
import Data.Maybe ( fromJust, catMaybes )
import Data.Map as M ( Map, keys, lookup, member, adjust, insert, (!), empty )
import Data.PriorityQueue.FingerTree as PQ ( PQueue, singleton, minViewWithKey, insert )

import Utils ( splitWhen, parseCoords )

-------------------------------------------------------------------------------
-----------------------------Types---------------------------------------------
-------------------------------------------------------------------------------

type Coord = (Int, Int)
type Dir = Int
data Node = Node { coord :: Coord,
                   dir   :: Dir,
                   line  :: Int, 
                   dis   :: Int }
    deriving Show

type LineType = Int
type InputType = Map Coord Int
type ResultType = Int

-------------------------------------------------------------------------------
-----------------------------Boilerplate---------------------------------------
-------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "What input file do you want to use?"
    file <- getLine
    lines <- readLinesFromFile ("inputs/" ++ file ++ ".txt")
    let parsedLines = parseLines1 lines
    putStrLn $ "Part 1: " ++ show (processInput1 parsedLines)
    --let parsed2Lines = map parseLine2 $ lines
    --let parsed2Lines = parseLines2 lines
    --putStrLn $ "Part 2: " ++ show (processInput2 parsed2Lines)

readLinesFromFile :: FilePath -> IO [String]
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (lines contents)

-------------------------------------------------------------------------------
-----------------------------Parsing-------------------------------------------
-------------------------------------------------------------------------------

parseLines1 :: [String] -> InputType
parseLines1 lines = parseCoords (\x -> Just $ digitToInt x) lines

parseLines2 :: [String] -> InputType
parseLines2 = parseLines1

-------------------------------------------------------------------------------
-----------------------------Processing----------------------------------------
-------------------------------------------------------------------------------

step :: Dir -> Coord -> Coord
step 0 (x, y) = (x, y-1)
step 1 (x, y) = (x+1, y)
step 2 (x, y) = (x, y+1)
step 3 (x, y) = (x-1, y)

neighbors :: InputType -> Int -> Int -> Node -> [Node]
neighbors m minLine maxLine (Node c dir line dis) = catMaybes $ map mkNode [(dir, line+1, 1), ((dir - 1) `mod` 4, minLine, minLine), ((dir + 1) `mod` 4, minLine, minLine)]
    where mkNode (dir', line', steps)  = let (dis', n@(x, y)) = move c steps dir' in 
                                         if x >= 0 && y >= 0 && x <= mx && y <= my && line' <= maxLine 
                                            then Just $ Node n dir' line' (dis + dis') 
                                            else Nothing
          mx = maximum $ map fst $ keys m
          my = maximum $ map snd $ keys m
          move c' 1 dir' = let n = step dir' c' in (fromJustInt $ M.lookup n m, n)
          move c' x dir' = let n = step dir' c' 
                               (d, e) = move n (x-1) dir' in 
                                (d + fromJustInt (M.lookup n m), e)
          fromJustInt Nothing = 0
          fromJustInt (Just x) = x

dijkstra :: (Node -> Bool) -> Map Coord Int -> Map Coord [(Dir, Int)] -> PQueue Int Node -> Int
dijkstra term m prev pq = let ((k, v), pq') = fromJust $ minViewWithKey pq in
                          if term v then k else dijkstra term m (addPrev v) (update pq' $ neighbors m 1 3 v)
    where update :: PQueue Int Node -> [Node] -> PQueue Int Node
          update pq []                        = pq
          update pq (n@((Node _ _ _ dis)):ns) = update (if inPrev n then pq else PQ.insert dis n pq) ns
          inPrev (Node c dir line _) = member c prev && elem (dir, line) (prev!c)
          addPrev (Node c dir line _) = if member c prev
                                           then adjust ((dir, line):) c prev
                                           else M.insert c [(dir, line)] prev

processInput1 :: InputType -> ResultType
processInput1 m = dijkstra (\(Node c _ _ _) -> c==(mx, my)) m M.empty $ PQ.singleton 0 $ Node (0, 0) 1 0 0
    where mx = maximum $ map fst $ keys m
          my = maximum $ map snd $ keys m

processInput2 :: InputType -> ResultType
processInput2 = processInput1