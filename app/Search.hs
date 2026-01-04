{-# LANGUAGE ScopedTypeVariables, FlexibleContexts  #-}

module Search where

import qualified Data.Set as S
import qualified Data.Heap as H

-- (Number of nodes expanded, Number of states stored at end of search)
data Stats = S !Int !Int deriving Show

breadthFirstTreeSearch :: (state -> [state]) -> state ->
                          (state -> Bool) -> (Maybe [state],Stats)
breadthFirstTreeSearch expandState startState isGoal
  | isGoal startState = (Just [startState],S 0 0)
  | otherwise = bfts [[startState]] [] (S 0 0)
  where
    bfts [] [] stats = (Nothing,stats)
    bfts [] new_frontier stats = bfts new_frontier [] stats
    bfts (this_path:rest_of_frontier) new_frontier (S n s) =
      let from_here = expandState $ head this_path
          goals_from_here = filter isGoal from_here
      in if null goals_from_here
         then bfts rest_of_frontier
              (map (:this_path) from_here ++ new_frontier)
              (S (n+1) (s-1+length from_here))
         else (Just $ head goals_from_here : this_path,S (n+1) s)

breadthFirstGraphSearch :: Ord state => (state -> [state]) -> state ->
                           (state -> Bool) -> (Maybe [state],Stats)
breadthFirstGraphSearch expandState startState isGoal
  | isGoal startState = (Just [startState],S 0 0)
  | otherwise = bfts [[startState]] [] (S.singleton startState) (S 0 0)
  where
    bfts [] [] _ stats = (Nothing,stats)
    bfts [] new_frontier explored_set stats
      = bfts new_frontier [] explored_set stats
    bfts (this_path:rest_of_frontier) new_frontier explored_set (S n s) =
      let from_here = filter (not . flip S.member explored_set) $
                      expandState $ head this_path
          goals_from_here = filter isGoal from_here
      in if null goals_from_here
         then bfts rest_of_frontier
              (map (:this_path) from_here ++ new_frontier)
              (S.union explored_set $ S.fromList from_here)
              (S (n+1) (s+length from_here))
         else (Just $ head goals_from_here : this_path,S (n+1) s)

uniformCostTreeSearch :: forall cost state. Real cost =>
                         (state -> [(state,cost)]) -> state ->
                         (state -> Bool) -> (Maybe ([state],cost),Stats)
uniformCostTreeSearch expandState startState isGoal =
  ucts (H.singleton (0,[startState]) :: H.MinPrioHeap cost [state]) (S 0 0)
  where
    ucts h stats@(S n s) = case H.view h of
      Nothing -> (Nothing,stats)
      Just ((current_cost,current_path),h') ->
        let current_state = head current_path
            from_here = expandState current_state
        in if isGoal current_state
           then (Just (current_path,current_cost),stats)
           else ucts (H.union h' $ H.fromList $
                      map (\(state,cost) -> (current_cost+cost,state:current_path)) from_here)
                (S (n+1) (s+length from_here))

uniformCostGraphSearch :: forall cost state. (Real cost, Ord state) =>
                         (state -> [(state,cost)]) -> state ->
                         (state -> Bool) -> (Maybe ([state],cost),Stats)
uniformCostGraphSearch expandState startState isGoal =
  ucgs (H.singleton (0,[startState]) :: H.MinPrioHeap cost [state])
  (S.empty) (S 0 0)
  where
    ucgs h explored_set stats@(S n s) = case H.view h of
      Nothing -> (Nothing,stats)
      Just ((current_cost,current_path),h') ->
        let current_state = head current_path
            from_here = filter (not . flip S.member explored_set . fst) $
                        expandState current_state
        in if isGoal current_state
           then (Just (current_path,current_cost),stats)
           else if S.member current_state explored_set
                then ucgs h' explored_set (S n (s-1))
                else ucgs (H.union h' $ H.fromList $
                           map (\(state,cost) -> (current_cost+cost,state:current_path)) from_here)
                     (S.insert current_state explored_set)
                     (S (n+1) (s-1+length from_here))

aStarGraphSearch :: forall cost state. (Real cost, Ord state) =>
                    (state -> [(state,cost)]) -> (state -> cost) -> state ->
                    (state -> Bool) -> (Maybe ([state],cost),Stats)
aStarGraphSearch expandState heuristic startState isGoal =
  ucgs (H.singleton (heuristic startState,(0,[startState])) ::
           H.MinPrioHeap cost (cost,[state]))
  (S.empty) (S 0 0)
  where
    ucgs h explored_set stats@(S n s) = case H.view h of
      Nothing -> (Nothing,stats)
      Just ((_,(current_cost,current_path)),h') ->
        let current_state = head current_path
            from_here = filter (not . flip S.member explored_set . fst) $
                        expandState current_state
        in if isGoal current_state
           then (Just (current_path,current_cost),stats)
           else if S.member current_state explored_set
                then ucgs h' explored_set (S n (s-1))
                else ucgs (H.union h' $ H.fromList $
                           map (\(state,cost) -> (current_cost+cost+heuristic state,
                                                  (current_cost+cost,state:current_path))) from_here)
                     (S.insert current_state explored_set)
                     (S (n+1) (s-1+length from_here))
