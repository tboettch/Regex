-- | Code for the graphviz integration
module Regex.Graphviz where

import Regex.Internal
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Graph.Inductive as Graph
import Data.GraphViz hiding (parse, toDot)
import Data.GraphViz.Attributes.Complete

-- | Converts a 'Regex' to dot format, suitable for rendering by graphviz.                               
toDot :: Regex -> DotGraph Graph.Node
toDot (Regex nfa) = graphToDot params (toGraph nfa)
    where params = nonClusteredParams {
                     globalAttributes = [ GraphAttrs {attrs = [RankDir FromLeft]} ],
                     fmtNode = (\(n, _) -> case n of 
                                                   1 -> [style filled, fillColor Red]
                                                   _ -> []),
                     fmtEdge = \(_, _, el) -> [toLabel el]
                   }
                   
-- | Converts an 'NFA' to a 'Graph.Gr' representation
toGraph :: NFA -> Graph.Gr String String 
toGraph nfa = Graph.mkGraph nodes edges
    where states = enumerate nfa
          nodes = map mkNode states
          edges = concatMap mkEdges states
          mkNode (NFA (Tag n _)) = (n, show n)
          mkEdges (NFA (Tag n (BlankState nfas)))   = map (\nfa' -> (n, getId nfa', ['ε']))(Set.toList (nfas))
          mkEdges (NFA (Tag n (MatchState c nfas))) = map (\nfa' -> (n, getId nfa', [c]))  (Set.toList (nfas))
          mkEdges (NFA (Tag _ FinalState))          = []

-- | Matches the given 'Regex' against the target, showing each step as a 'DotGraph'.
simulate :: Regex -- ^ The regular expression to simulate.
         -> MatchTarget -- ^ The string that should be matched against the expression.
         -> [DotGraph Graph.Node] -- ^ A list of 'DotGraph's representing the steps the algorithm took to reach a decision.
simulate (Regex nfa) str = map dotify intermediates 
    where graph = toGraph nfa
          dotify :: IntSet.IntSet -> DotGraph Graph.Node
          dotify nodes = graphToDot params graph
            where params = nonClusteredParams {
                            globalAttributes = [ GraphAttrs {attrs = [RankDir FromLeft]} ],
                            fmtNode = (\(n, _) -> if IntSet.member n nodes then [style filled, fillColor Red] else []),
                            fmtEdge = \(_, _, el) -> [toLabel el]
                           }
          getNodes :: NfaSet -> IntSet.IntSet
          getNodes nfas = IntSet.fromList $ map getId (Set.toList nfas)
          intermediates :: [IntSet.IntSet]
          intermediates = go (travel $ Set.singleton nfa) str
          go :: NfaSet -> String -> [IntSet.IntSet]
          go nfas []     = [getNodes nfas]
          go nfas (x:xs) = (getNodes nfas):(go (advance nfas x) xs)
          
--TODO: Move elsewhere.
-- | Temporary: do not use.
outputDots :: [DotGraph Graph.Node] -> IO ()
outputDots xs = sequence_ $ map dotify $ zip [0..] xs
    where dotify (n,g) = runGraphviz g Png ("C:\\temp\\graphs\\" ++ (show n) ++ ".png")

