{-# LANGUAGE ScopedTypeVariables, FlexibleContexts  #-}

{-
Breadth first: each step consists of expanding the entire frontier,
checking if a goal has been reached, and, if not, constructing a new
frontier.  This can be done incrementally with a FIFO queue.
Neighbors is a list of states and actions. Costs are implicitly 1.
Graph variant checks new states against a list of previously visited
states; tree variant does not.  Tree variant is not recommended on
graphs, as the space savings of not storing the explored set are
smaller than the cost to store the frontier, and the time costs of
revisiting nodes are potentially very large.

Uniform cost: each step consists of locating the node on the frontier
with the lowest cost, checking if it is a goal, removing it from the
frontier, expanding it, and adding its successors to the frontier.
Neighbors is a list of states, actions, and costs.
Graph variant checks new states against a list of previously visited
states; tree variant does not.  Graph variant also needs to deal with
the possibility that a node on the frontier may be generated at a
later point in the search with a lower cost.  (The explored set is
always generated in increasing order of cost.)  This is accomplished
by applying the goal test when a state is selected for expansion, not
when it is first generated.  As in breadth first search, the tree
variant is not recommended on graphs.

Depth first: each step consists of expanding the deepest unexpanded
node, checking if a goal has been reached, and backing up only when
there are no successors.  Graph variant keeps track of all visited
states, but it never makes sense to use "depth first graph search"
in this sense because breadth first is better.  "Tree" variant checks
either nothing or nodes along the current history for duplication.
Neighbors is a list of states and actions. Costs are implicitly 1.

Closely related depth-limited search adds a depth limit, and informs
the user if no solution exists, or if the cutoff was reached.

Iterative deepening depth first: Repeat depth-limited search as long
as the result is "cutoff reached", increasing depth limit by 1 each
time.

A*: an informed search; improvement on uniform cost search that also
takes into account a lower bound on the cost from the current state
to the goal.

Iterative deepening A*: practical when step costs are unit.
RBFS: see book... complicated...
(Simplified) Memory-bounded A*... this is getting really complex...

Bidirectional search: a form of breadth first search when the goal is
known ahead of time, and the point of the search is only to find the
shortest path.  Requires predecessors of a state to be computable
(always true if state space is an undirected graph).  Can be done in
any of the breadth first or uniform cost variants, including A*.
-}

module Search where

import qualified Data.Set as S
import qualified Data.Heap as H

-- (Number of nodes expanded, Number of states stored at end of search)
data Stats = S !Int !Int deriving Show

breadthFirstTreeSearch :: (state -> [state]) -> state ->
                          (state -> Bool) -> (Maybe [state],Stats)
breadthFirstTreeSearch expand start isGoal
  | isGoal start = (Just [start],S 0 0)
  | otherwise = bfts [[start]] [] (S 0 0)
    where
      bfts [] [] stats = (Nothing,stats)
      bfts [] new_frontier stats = bfts new_frontier [] stats
      bfts (this_path:rest_of_frontier) new_frontier (S n s) =
        let from_here = expand $ head this_path
            goals_from_here = filter isGoal from_here
        in if null goals_from_here
           then bfts rest_of_frontier
                (map (:this_path) from_here ++ new_frontier)
                (S (n+1) (s-1+length from_here))
           else (Just $ head goals_from_here : this_path,S (n+1) s)

breadthFirstGraphSearch :: Ord state => (state -> [state]) -> state ->
                           (state -> Bool) -> (Maybe [state],Stats)
breadthFirstGraphSearch expand start isGoal
  | isGoal start = (Just [start],S 0 0)
  | otherwise = bfts [[start]] [] (S.singleton start) (S 0 0)
    where
      bfts [] [] _ stats = (Nothing,stats)
      bfts [] new_frontier explored_set stats
        = bfts new_frontier [] explored_set stats
      bfts (this_path:rest_of_frontier) new_frontier explored_set (S n s) =
        let from_here = filter (not . flip S.member explored_set) $
                        expand $ head this_path
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
uniformCostTreeSearch expand start isGoal =
  ucts (H.singleton (0,[start]) :: H.MinPrioHeap cost [state]) (S 0 0)
    where
      ucts h stats@(S n s) = case H.view h of
        Nothing -> (Nothing,stats)
        Just ((current_cost,current_path),h') ->
          let current_state = head current_path
              from_here = expand current_state
          in if isGoal current_state
             then (Just (current_path,current_cost),stats)
             else ucts (H.union h' $ H.fromList $
                  map (\(state,cost) -> (current_cost+cost,state:current_path)) $ from_here)
                  (S (n+1) (s+length from_here))

uniformCostGraphSearch :: forall cost state. (Real cost, Ord state) =>
                         (state -> [(state,cost)]) -> state ->
                         (state -> Bool) -> (Maybe ([state],cost),Stats)
uniformCostGraphSearch expand start isGoal =
  ucgs (H.singleton (0,[start]) :: H.MinPrioHeap cost [state])
       (S.empty) (S 0 0)
    where
      ucgs h explored_set stats@(S n s) = case H.view h of
        Nothing -> (Nothing,stats)
        Just ((current_cost,current_path),h') ->
          let current_state = head current_path
              from_here = filter (not . flip S.member explored_set . fst) $
                          expand current_state
          in if isGoal current_state
             then (Just (current_path,current_cost),stats)
             else if S.member current_state explored_set
                  then ucgs h' explored_set (S n (s-1))
                  else ucgs (H.union h' $ H.fromList $
                             map (\(state,cost) -> (current_cost+cost,state:current_path))
                             from_here)
                       (S.insert current_state explored_set)
                       (S (n+1) (s-1+length from_here))

aStarGraphSearch :: forall cost state. (Real cost, Ord state) =>
                    (state -> [(state,cost)]) -> (state -> cost) -> state ->
                    (state -> Bool) -> (Maybe ([state],cost),Stats)
aStarGraphSearch expand lower_bound start isGoal =
  ucgs (H.singleton (lower_bound start,(0,[start])) ::
           H.MinPrioHeap cost (cost,[state]))
       (S.empty) (S 0 0)
    where
      ucgs h explored_set stats@(S n s) = case H.view h of
        Nothing -> (Nothing,stats)
        Just ((_,(current_cost,current_path)),h') ->
          let current_state = head current_path
              from_here = filter (not . flip S.member explored_set . fst) $
                          expand current_state
          in if isGoal current_state
             then (Just (current_path,current_cost),stats)
             else if S.member current_state explored_set
                  then ucgs h' explored_set (S n (s-1))
                  else ucgs (H.union h' $ H.fromList $
                             map (\(state,cost) -> (current_cost+cost+lower_bound state,
                                             (current_cost+cost,state:current_path)))
                             from_here)
                       (S.insert current_state explored_set)
                       (S (n+1) (s-1+length from_here))
