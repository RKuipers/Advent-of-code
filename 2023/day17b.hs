--module Day17b  ( part1  , part2  ) where

import           Data.Array.Unboxed as UA (UArray, bounds, inRange, (!), array)
import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.HashPSQ       as Q (HashPSQ, insert, singleton, null, minView)
import           Data.Map           as M (Map, elems, empty, insert, lookup,
                                          singleton, member, (!))
import Data.Char (digitToInt)
import           Data.Array.IArray  (IArray)
import           Data.Maybe         (fromJust, mapMaybe)
--import           Helpers.Parsers    (digitArrayFromString)
--import           Helpers.Search     (dijkstraMech)
import           Linear.V2          (V2 (..))

data Crucible =
  Crucible
    { pos :: Pos
    , dir :: Pos
    , acc :: Int
    }
  deriving (Show, Eq, Ord)

type Blocks = UArray Pos Int

type Pos = V2 Int

instance Hashable Crucible where
  hashWithSalt salt (Crucible (V2 a b) (V2 c d) e) =
    salt + a * 10 ^ 16 + b * 10 ^ 12 + c * 10 ^ 8 + d * 10 ^ 4 + e

make2DArray :: IArray UArray a => [[a]] -> UArray (V2 Int) a
make2DArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = length (head l) - 1
    height = length l - 1

digitArrayFromString :: String -> UArray (V2 Int) Int
digitArrayFromString = make2DArray . map (map digitToInt) . lines

dijkstraMech ::
     (Hashable k, Ord k, Show k, Num p, Ord p)
  => HashPSQ k p k
  -> Map k p
  -> Map k k
  -> (k -> [(k, p)])
  -> (k -> Bool)
  -> (k, (Map k p, Map k k))
dijkstraMech queue dists paths neighbours isGoal
  | Q.null queue = error "Goal not found"
  | isGoal curKey = (curKey, (dists, paths))
  | otherwise = dijkstraMech newQueue newDists newPaths neighbours isGoal
  where
    (curKey, estDist, _, rest) = fromJust (minView queue)
    toConsider = mapMaybe consider (neighbours curKey)
    newQueue = foldl (\a (b, c) -> Q.insert b c b a) rest toConsider
    newDists = foldl (\a (b, c) -> M.insert b c a) dists toConsider
    newPaths = foldl (\a (b, _) -> M.insert b curKey a) paths toConsider
    consider (aKey, anEdge)
      | not (M.member aKey dists) || estDist + anEdge < dists M.! aKey =
        Just (aKey, estDist + anEdge)
      | otherwise = Nothing

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

minMove1 = 0

maxMove1 = 3

minMove2 = 4

maxMove2 = 10

left :: Pos -> Pos
left (V2 x y) = V2 y (-x)

right :: Pos -> Pos
right (V2 x y) = V2 (-y) x

nextMoves :: Crucible -> [Crucible]
nextMoves (Crucible p d a) =
  Crucible (p + d) d (a + 1) :
  map (\x -> Crucible (p + x d) (x d) 1) [left, right]

heatLoss :: Blocks -> Pos -> Pos -> Int
heatLoss blocks _ p = blocks UA.! p

moves :: Int -> Int -> Blocks -> Crucible -> [(Crucible, Int)]
moves minMoves maxMoves blocks c@(Crucible p d nm) =
  map (\x -> (x, blocks UA.! pos x)) .
  filter (\(Crucible np _ na) -> inRange (bounds blocks) np && na <= maxMoves) $
  next
  where
    next
      | nm < minMoves = [Crucible (p + d) d (nm + 1)]
      | otherwise = nextMoves c

part1 :: Bool -> String -> String
part1 _ input = show dijkVal
  where
    blocks = digitArrayFromString input
    startPos = Crucible start east 0
    (start, endGoal) = bounds blocks
    (actualGoal, (dijVals, _)) =
      dijkstraMech
        (Q.singleton startPos 0 startPos)
        (M.singleton startPos 0)
        M.empty
        (moves minMove1 maxMove1 blocks)
        ((==) endGoal . pos)
    dijkVal = fromJust . M.lookup actualGoal $ dijVals

part2 :: Bool -> String -> String
part2 _ input = show dijkVal
  where
    blocks = digitArrayFromString input
    startPosEast = Crucible start east 0
    startPosSouth = Crucible start south 0
    (start, endGoal) = bounds blocks
    (actualGoal, (dijVals, _)) =
      dijkstraMech
        (Q.insert startPosEast 0 startPosEast $
         Q.singleton startPosSouth 0 startPosSouth)
        (M.insert startPosEast 0 $ M.singleton startPosSouth 0)
        M.empty
        (moves minMove2 maxMove2 blocks)
        (\c -> pos c == endGoal && acc c >= 4)
    dijkVal = fromJust . M.lookup actualGoal $ dijVals

readLinesFromFile :: FilePath -> IO String
readLinesFromFile filePath = do
    contents <- readFile filePath
    return (contents)

main :: IO ()
main = do
  lines <- readFile "inputs/day_17_large.txt"
  putStrLn $ "Part 1: " ++ show (part2 True lines)