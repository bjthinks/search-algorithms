module Main where

import Data.List (elemIndex)
import Data.Array
import Data.Maybe (fromJust)
import Search

data SlidingBlockPuzzle = SBP Int Int Int (Array Int (Array Int Int))
                          deriving (Eq,Ord)

instance Show SlidingBlockPuzzle where
  show (SBP _ _ _ b) = ('\n':) $ unlines $
                       map (unwords . map show . elems) (elems b)

solvedSlidingBlockPuzzle :: Int -> SlidingBlockPuzzle
solvedSlidingBlockPuzzle n
  | n > 0 = SBP n 0 0 $
            listArray (0,n-1) [listArray (0,n-1) [a..b] |
                               (a,b) <- zip [0,n..n^2-n] [n-1,2*n-1..n^2-1]]
  | otherwise = error "Sliding block puzzle of nonpositive size"

expandSlidingBlockPuzzle :: SlidingBlockPuzzle -> [SlidingBlockPuzzle]
expandSlidingBlockPuzzle p@(SBP n r c d)
  = map swap $ filter valid $ [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]
  where
    valid (x,y) = x >= 0 && x < n && y >= 0 && y < n
    swap (x,y)
      | x == r = SBP n x y $ d // [(r,(d!r) // [(y,0),(c,(d!r)!y)])]
      | otherwise = SBP n x y $ d // [(r,(d!r) // [(c,(d!x)!c)]),
                                      (x,(d!x) // [(c,(d!r)!c)])]

misplacedDistance :: SlidingBlockPuzzle -> Int
misplacedDistance (SBP n _ _ d)
  = length $ filter ((/=0) . snd) $ filter (uncurry (/=)) $ zip [0..n^2-1] (concat $ map elems $ elems d)

manhattanDistance :: SlidingBlockPuzzle -> Int
manhattanDistance (SBP n _ _ arr)
  = sum [l1distance (properLocation k) (actualLocation k) | k <- [1..n^2-1]]
  where
    l1distance (a,b) (c,d) = abs (a-c) + abs (b-d)
    properLocation k = (div k n, mod k n)
    actualLocation k = fromJust $ lookup k $ assocs_arr
    assocs_arr = concat $ map (\(r,z) -> [(k,(r,c)) | (c,k) <- assocs z]) $
                 assocs arr

gaschnigDistance :: SlidingBlockPuzzle -> Int
gaschnigDistance p@(SBP n r c d)
  | d_list == [0..n^2-1] = 0
  | otherwise = 1 + gaschnigDistance (swap (r',c'))
    where
      d_list = concat (map elems (elems d))
      badIndex = bi (tail d_list) 1
      bi (x:xs) y
        | x == y = bi xs (y+1)
        | otherwise = y
      zzpos = fromJust $ elemIndex (n*r+c) d_list
      (r',c')
        | r == 0 && c == 0 = (div badIndex n,mod badIndex n)
        | otherwise = (div zzpos n,mod zzpos n)
      swap (x,y)
        | x == r = SBP n x y $ d // [(r,(d!r) // [(y,0),(c,(d!r)!y)])]
        | otherwise = SBP n x y $ d // [(r,(d!r) // [(c,(d!x)!y)]),
                                        (x,(d!x) // [(y,0)])]

main :: IO ()
main = do
  let x1 = SBP 2 0 0 $ listArray (0,1) $ map (listArray (0,1)) [[0,3],[1,2]]
  putStrLn "2x2 puzzle with breadth first search:"
  print $ breadthFirstTreeSearch {-(map (\z -> (z,1)) .-}( expandSlidingBlockPuzzle) x1 (==solvedSlidingBlockPuzzle 2)

  let x2 = SBP 3 1 2 $ listArray (0,2) $ map (listArray (0,2)) [[3,4,7],[5,1,0],[6,8,2]]
  putStrLn "3x3 puzzle with A* search using misplaced distance:"
  print $ aStarGraphSearch (map (\z -> (z,1)) . expandSlidingBlockPuzzle) misplacedDistance x2 (==solvedSlidingBlockPuzzle 3)
  putStrLn "3x3 puzzle with A* search using Gaschnig distance:"
  print $ aStarGraphSearch (map (\z -> (z,1)) . expandSlidingBlockPuzzle) gaschnigDistance x2 (==solvedSlidingBlockPuzzle 3)
  putStrLn "3x3 puzzle with A* search using Manhattan distance:"
  print $ aStarGraphSearch (map (\z -> (z,1)) . expandSlidingBlockPuzzle) manhattanDistance x2 (==solvedSlidingBlockPuzzle 3)

  putStrLn "4x4 puzzle using max of Manhattan & Gaschnig distances (slow):"
  let x3 = SBP 4 1 3 $ listArray (0,3) $ map (listArray (0,3)) [[3,10,4,7],[13,5,1,0],[15,11,9,14],[12,6,8,2]]
  print $ aStarGraphSearch (map (\z -> (z,1)) . expandSlidingBlockPuzzle) (\z -> max (manhattanDistance z) (gaschnigDistance z)) x3 (==solvedSlidingBlockPuzzle 4)
