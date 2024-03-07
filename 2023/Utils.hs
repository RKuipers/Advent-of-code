module Utils where

import Data.Map ( fromList, Map )

type Coord = (Int, Int)

--Given predicate p and list s, split s around each element satisfying p
--Removes all elements that satisfy predicate
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

--Given a function that turns (some) chars into a, create a map (i.e. dictionary)
--of those a's, with the Coords (Int, Int) as the key
parseCoords :: (Char -> Maybe a) -> [String] -> Map Coord a
parseCoords f ls = fromList $ concat (go 0 f ls)
    where go y f []     = []
          go y f (l:ls) = go2 0 y f l: go (y+1) f ls
          go2 x y f []     = []
          go2 x y f (c:cs) = case f c of
                               Nothing -> go2 (x+1) y f cs
                               Just a  -> ((x, y), a) : go2 (x+1) y f cs

--Calculates the area of a shape represented by integer coordiantes
shoeLace :: [Coord] -> Int
shoeLace []                     = 0
shoeLace ((xa, ya):(xb, yb):xs) = xa * yb - xb * ya + shoeLace xs

--Return whether a given Int is even
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0