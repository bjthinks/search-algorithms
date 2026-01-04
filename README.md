# Search algorithms implemented in Haskell

Each of these algorithms reports the number of search nodes that
were expanded and the number of states that were stored in memory
when the search finishes.

```
data SearchStats = SearchStats !Int !Int deriving Show
```

Breadth first: each step consists of expanding the entire frontier,
checking if a goal has been reached, and, if not, constructing a new
frontier.  This can be done incrementally with a FIFO queue.
Neighbors is a list of states and actions. Costs are implicitly 1.
Graph variant checks new states against a list of previously visited
states; tree variant does not.  Tree variant is not recommended on
graphs, as the space savings of not storing the explored set are
smaller than the cost to store the frontier, and the time costs of
revisiting nodes are potentially very large.

```
breadthFirstTreeSearch :: (state -> [state]) -> state ->
                          (state -> Bool) -> (Maybe [state],SearchStats)
breadthFirstTreeSearch expandState startState isGoal

breadthFirstGraphSearch :: Ord state => (state -> [state]) -> state ->
                           (state -> Bool) -> (Maybe [state],SearchStats)
breadthFirstGraphSearch expandState startState isGoal
```

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

```
uniformCostTreeSearch :: forall cost state. Real cost =>
                         (state -> [(state,cost)]) -> state ->
                         (state -> Bool) -> (Maybe ([state],cost),SearchStats)
uniformCostTreeSearch expandState startState isGoal

uniformCostGraphSearch :: forall cost state. (Real cost, Ord state) =>
                         (state -> [(state,cost)]) -> state ->
                         (state -> Bool) -> (Maybe ([state],cost),SearchStats)
uniformCostGraphSearch expandState startState isGoal
```

Depth first: each step consists of expanding the deepest unexpanded
node, checking if a goal has been reached, and backing up only when
there are no successors.  Graph variant keeps track of all visited
states, but it never makes sense to use "depth first graph search"
in this sense because breadth first is better.  "Tree" variant checks
either nothing or nodes along the current history for duplication.
Neighbors is a list of states and actions. Costs are implicitly 1.
Plain depth first search is not implemented because it isn't a very
useful algorithm for most problems.

Closely related depth-limited search adds a depth limit, and informs
the user if no solution was found. The main reason to use a
depth-limited search is that it takes very little memory to run.

```
depthLimitedSearch :: (state -> [state]) -> state ->
                      (state -> Bool) -> Int -> (Maybe [state],SearchStats)
depthLimitedSearch expandState startState isGoal depthLimit
```

Iterative deepening depth first: Repeat depth-limited search as long
as the result is "cutoff reached", increasing depth limit by 1 each
time. This is an efficient search algorithm for finding an optimal
solution when the states that need to be explored are too large to
hold in memory.

```
iterativeDeepeningSearch :: (state -> [state]) -> state ->
                            (state -> Bool) -> (Maybe [state],SearchStats)
iterativeDeepeningSearch expandState startState isGoal
```

A*: an informed search; improvement on uniform cost search that also
takes into account a lower bound on the cost from the current state
to the goal.

```
aStarGraphSearch :: forall cost state. (Real cost, Ord state) =>
                    (state -> [(state,cost)]) -> (state -> cost) -> state ->
                    (state -> Bool) -> (Maybe ([state],cost),SearchStats)
aStarGraphSearch expandState heuristic startState isGoal
```

There are also Iterative deepening A* and Memory-bounded A* searches.

Bidirectional search: a form of breadth first search when the goal is
known ahead of time, and the point of the search is only to find the
shortest path.  Requires predecessors of a state to be computable
(always true if state space is an undirected graph).  Can be done in
any of the breadth first or uniform cost variants, including A*.

These programs are in the Public Domain.
