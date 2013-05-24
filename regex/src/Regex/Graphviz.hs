-- | Code for the graphviz integration
module Regex.Graphviz where

import Regex.Internal
import Regex.NFA
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Graph.Inductive as Graph
import Data.GraphViz hiding (parse, toDot)
import Data.GraphViz.Attributes.Complete
import Control.Monad (mapM_)

-- |Id for a fake, invisible node used to let graphviz show an edge pointing to the initial state.
dummyNodeId :: Int          
dummyNodeId = -1

-- |Default parameters for rendering with Graphviz
baseParams :: GraphvizParams Int String String () String
baseParams = let finalId = 0 in nonClusteredParams {
    globalAttributes = [ GraphAttrs {attrs = [RankDir FromLeft]} ],
    fmtNode = (\(i, _) -> (if i == dummyNodeId then [style invis] else []) ++
                          (if i == finalId then [shape DoubleCircle] else [])),
    fmtEdge = \(_, _, el) -> [toLabel el]
  }
                   
-- | Converts an 'NFA' to a 'Graph.Gr' representation
toGraph :: NFA -> Graph.Gr String String 
toGraph nfa = Graph.mkGraph nodes edges
    where states = enumerate nfa
          nodes = dummyNode:(map mkNode states)
          edges = initialEdge:(concatMap mkEdges states)
          dummyNode = (dummyNodeId, show dummyNodeId)
          initialEdge = (dummyNodeId, getId nfa, "")
          mkNode (NFA (Tag n _)) = (n, show n)
          mkEdges (NFA (Tag n (BlankState nfas)))   = map (\nfa' -> (n, getId nfa', "ε"))(Set.toList (nfas))
          mkEdges (NFA (Tag n (MatchState c nfas))) = map (\nfa' -> (n, getId nfa', [c]))  (Set.toList (nfas))
          mkEdges (NFA (Tag _ FinalState))          = []
          
-- | Converts a 'Regex' to dot format, suitable for rendering by graphviz.                               
toDot :: Regex -> DotGraph Graph.Node
toDot (Regex nfa) = graphToDot baseParams (toGraph nfa)

-- | Matches the given 'Regex' against the target, showing each step as a 'DotGraph'.
simulate :: Regex -- ^ The regular expression to simulate.
         -> MatchTarget -- ^ The string that should be matched against the expression.
         -> [DotGraph Graph.Node] -- ^ A list of 'DotGraph's representing the steps the algorithm took to reach a decision.
simulate (Regex nfa) str = map (dotify . getNodes) steps 
    where -- | Renders a snapshot of the NFA based on the specified active states.
          dotify :: IntSet.IntSet -> DotGraph Graph.Node
          dotify nodes = graphToDot params $ toGraph nfa
            where params = baseParams {
              -- Highlight active nodes
              fmtNode = \node@(i,_) ->(if IntSet.member i nodes then [style filled, fillColor Red] else []) ++ (fmtNode baseParams node)
            }
          -- | Steps taken during matching. Each element is a set of states active during that step.
          steps :: [NfaSet]
          steps = scanl advance (travel $ Set.singleton nfa) str
          -- | Gets the IDs of the nodes in the given set.
          getNodes :: NfaSet -> IntSet.IntSet
          getNodes = IntSet.fromList . (map getId) . Set.toList

