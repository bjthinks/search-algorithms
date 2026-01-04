module Main where

import Data.List
import Data.Maybe (catMaybes)
import Search

data Location = Location Int Int
  deriving (Show,Eq,Ord)

isValidLoc :: Location -> Bool
isValidLoc (Location x y)
  | x < 0 = False
  | x > 4 = False
  | y < 0 = False
  | y > 5 = False
  | x == 0 && y >= 3 = False
  | x == 4 && y >= 3 = False
  | x == 1 && y == 5 = False
  | x == 2 && y == 0 = False
  | x == 3 && y == 5 = False
  | otherwise = True

instance Num Location where
  Location a b + Location c d = Location (a+c) (b+d)
  Location a b * Location c d = Location (a*c) (b*d)
  negate (Location a b) = Location (negate a) (negate b)
  abs (Location a b) = Location (abs a) (abs b)
  signum (Location a b) = Location (signum a) (signum b)
  fromInteger a = Location (fromInteger a) (fromInteger a)

data MasterSwordPuzzle =
  MSP { linkLocation :: Location
      , fstGLocation :: Location
      , sndGLocation :: Location
      } deriving (Eq,Ord)

instance Show MasterSwordPuzzle where
  show p =
    let link = linkLocation p
        fstG = fstGLocation p
        sndG = sndGLocation p
    in
     ('\n' :) $
     intercalate "\n" $
     map (intersperse ' ') $
     [
       [
         let here = Location x y
         in if not (isValidLoc here) then '-' else
            if here == link          then 'L' else
            if here == fstG          then '1' else
            if here == sndG          then '2' else
            'O'
       | x <- [0..4] ]
     | y <- [0..5] ]

isValid :: MasterSwordPuzzle -> Bool
isValid p =
  let link = linkLocation p
      fstG = fstGLocation p
      sndG = sndGLocation p
  in isValidLoc link &&
     isValidLoc fstG &&
     isValidLoc sndG &&
     link /= fstG &&
     link /= sndG &&
     fstG /= sndG

moveBy :: MasterSwordPuzzle -> Location -> Maybe MasterSwordPuzzle
-- it's more complicated than this!!!
-- if Link's target position is valid, then the move can occur
-- when the move occurs, check if p2 and p3 are valid
-- if so, the result is p3
-- otherwise, it is still complicated.....
moveBy p delta =
  let newLinkLocation = linkLocation p + delta
      intermediateState = p { linkLocation = newLinkLocation }
      newFstGLocation = fstGLocation p - delta
      newSndGLocation = sndGLocation p + delta
      bump = newFstGLocation == newSndGLocation ||
             newFstGLocation == sndGLocation p ||
             newSndGLocation == fstGLocation p
      splat = newLinkLocation == newFstGLocation ||
              newLinkLocation == newSndGLocation
      afterFst = intermediateState { fstGLocation = newFstGLocation }
      afterSnd = intermediateState { sndGLocation = newSndGLocation }
      afterBoth = afterFst { sndGLocation = newSndGLocation }
  in if not (isValid intermediateState) then Nothing else
       if bump then Just intermediateState else
         if splat then Nothing else
           case (isValid afterFst,isValid afterSnd) of
             (False,False) -> Just intermediateState
             (False,True) -> Just afterSnd
             (True,False) -> Just afterFst
             (True,True) -> Just afterBoth

possibleDeltas :: [Location]
possibleDeltas = [Location (-1) 0,Location 1 0,Location 0 (-1),Location 0 1]

expandMasterSwordPuzzle :: MasterSwordPuzzle -> [MasterSwordPuzzle]
expandMasterSwordPuzzle p = catMaybes $ map (moveBy p) possibleDeltas

startMasterSwordPuzzle :: MasterSwordPuzzle
startMasterSwordPuzzle =
  MSP { linkLocation = Location 2 3
      , fstGLocation = Location 2 1
      , sndGLocation = Location 2 5 }

isGoalMasterSwordPuzzle :: MasterSwordPuzzle -> Bool
isGoalMasterSwordPuzzle p =
  (fstGLocation p == Location 1 1 && sndGLocation p == Location 3 1) ||
  (fstGLocation p == Location 3 1 && sndGLocation p == Location 1 1)

manhattanHeuristic :: MasterSwordPuzzle -> Int
manhattanHeuristic p =
  let f = fstGLocation p
      s = sndGLocation p
      g1 = Location 1 1
      g2 = Location 3 1
      manhattanDist (Location a b) (Location c d) = abs (a-c) + abs (b-d)
  in min (max (manhattanDist f g1) (manhattanDist s g2))
     (max (manhattanDist f g2) (manhattanDist s g1))

-- Minimum for the two assignments of goals:
-- max of x distance + max of y distance
smarterHeuristic :: MasterSwordPuzzle -> Int
smarterHeuristic p =
  let f = fstGLocation p
      s = sndGLocation p
      g1 = Location 1 1
      g2 = Location 3 1
      sumOfMaxes (Location a b) (Location c d) =
        max (abs a) (abs c) + max (abs b) (abs d)
  in min (sumOfMaxes (f-g1) (s-g2)) (sumOfMaxes (f-g2) (s-g1))

-- Minimum for the two assignments of goals:
-- x_heuristic + y_heuristic
-- x_heuristic is larger of x coords if opposite sign, or sum if same sign
brainyHeuristic :: MasterSwordPuzzle -> Int
brainyHeuristic p =
  let f = fstGLocation p
      s = sndGLocation p
      g1 = Location 1 1
      g2 = Location 3 1
      brainy (Location a b) (Location c d) = br a c + br b d
      br x y
        | signum x /= signum y = max (abs x) (abs y)
        | otherwise = abs (x+y)
  in min (brainy (f-g1) (s-g2)) (brainy (f-g2) (s-g1))

-- Note: solutions are
-- R D U U U L L D D D R U, or
-- L D U U U R R D D D L U

main :: IO ()
main = do
  putStrLn "Breadth first search:"
  print $ breadthFirstGraphSearch expandMasterSwordPuzzle startMasterSwordPuzzle isGoalMasterSwordPuzzle

  putStrLn "A* search with manhattan heuristic:"
  print $ aStarGraphSearch (map (\x -> (x,1)) . expandMasterSwordPuzzle) manhattanHeuristic startMasterSwordPuzzle isGoalMasterSwordPuzzle

  putStrLn "A* search with smarter heuristic:"
  print $ aStarGraphSearch (map (\x -> (x,1)) . expandMasterSwordPuzzle) smarterHeuristic startMasterSwordPuzzle isGoalMasterSwordPuzzle

  putStrLn "A* search with brainy heuristic:"
  print $ aStarGraphSearch (map (\x -> (x,1)) . expandMasterSwordPuzzle) brainyHeuristic startMasterSwordPuzzle isGoalMasterSwordPuzzle
