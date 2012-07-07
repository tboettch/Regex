module Regex (Regex, compile, matches, match, toDot, simulate, outputDots) where

import Data.List (foldl', intersperse)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.Monad.State
import qualified Data.Graph.Inductive as Graph
import qualified Data.GraphViz as GV
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands

-- Utility function for flattening the results of Set.map when f produces more sets.
unionMap :: (Ord a, Ord b) =>  (a -> Set.Set b) -> Set.Set a -> Set.Set b
unionMap f s = Set.unions $ map f (Set.toList s)

type RawRegex = String
type MatchTarget = [Alphabet]

-- Type representing the set of valid characters that can be matched by a Regex.
type Alphabet = Char
-- Regular expression type that can be matched against using
-- TODO: Remove show
newtype Regex = Regex NFA deriving Show

-- Compiles the given String into a Regex that can be matched against.
-- TODO: Error reporting
compile :: RawRegex -> Regex
compile = (Regex).buildNFA.parse

-- Abstract syntax tree for parsed regular expressions.
-- TODO: Extend to allow marking of matching groups, with indexes.
data AST = Empty
         | Lit Alphabet
         | Star AST
         | Concat AST AST
         | Or AST AST
         deriving Show

-- Converts a raw string into an AST.
-- TODO: Error reporting
parse :: RawRegex -> AST
parse input = case stackParse [] input of
                (ast, []) -> ast

type TokenStack = [AST]
-- Input: Stack of tokens processed so far and the remainder of the input string.
-- Output: AST and the unparsed remainder of the input string.
-- TODO: Error reporting
stackParse :: TokenStack -> RawRegex -> (AST, RawRegex)
stackParse _      ('\\':[])      = undefined
stackParse ts     ('\\':a:as)    = stackParse ((Lit a):ts) as
--TODO: Mark the AST parsed as being part of a matching group, indexed appropriately.
stackParse ts     ('(':as)       = case stackParse [] as of
                                    (x, as') -> stackParse (x:ts) as'
stackParse []     (')':_)        = undefined                                   
stackParse ts     (')':as)       = (concatStack ts, as)
stackParse []     ('*':_)        = undefined
stackParse (x:ts) ('*':as)       = stackParse ((Star x):ts) as
stackParse []     ('+':_)        = undefined
stackParse (x:ts) ('+':as)       = stackParse ((Concat x (Star x)):ts) as
stackParse []     ('?':_)        = undefined
stackParse (x:ts) ('?':as)       = stackParse ((Or x Empty):ts) as
stackParse []     ('|':_)        = undefined
stackParse ts     ('|':as)       = case (stackParse ts [], stackParse [] as) of
                                    ((left, []), (right, remainder)) -> stackParse ((Or left right):[]) remainder
stackParse ts     (a:as)         = stackParse ((Lit a):ts) as
stackParse []     []             = (Empty, [])
stackParse ts     []             = (concatStack ts, [])
  
-- | Concatenates the contents of a stack
concatStack :: [AST] -> AST
concatStack ts = foldr1 Concat (reverse ts)

buildNFA :: AST -> NFA
buildNFA ast = evalState (go ast (Set.singleton final)) 1 
    where final = NFA (Tag 0 FinalState)
          -- Arguments: the current subtree being evaluated, the set of output states from this subtree.
          go :: AST -> NfaSet -> State Int NFA
          go Empty        outs = makeNFA $ BlankState outs
          go (Lit c)      outs = makeNFA $ MatchState c outs
          go (Or l r)     outs = do left <- go l outs
                                    right <- go r outs
                                    makeNFA $ BlankState (Set.fromList [left, right])
          go (Concat l r) outs = do right <- go r outs
                                    go l (Set.singleton right)
          go (Star inner) outs = do n <- get
                                    let root = NFA(Tag n (BlankState(rest `Set.insert` outs)))
                                        (rest, n') = runState (go inner (Set.singleton root)) (n+1)
                                    put n'
                                    return root
          makeNFA :: NFAState -> State Int NFA
          makeNFA state = do n <- get
                             put (n+1)
                             return $ NFA (Tag n state)

matches :: Regex -> MatchTarget -> Bool
matches r s = case match r s of 
                Nothing -> False
                Just _  -> True

--TODO: Matching groups
match :: Regex -> MatchTarget -> Maybe [MatchTarget]
match (Regex nfa) s = go (travel $ Set.singleton nfa) s
    where go :: NfaSet -> String -> Maybe [String]
          go nfas []     = if final `Set.member` nfas
                                 then Just []
                                 else Nothing
          go nfas (x:xs) = go (advance nfas x) xs
          --For now, the final state always has a tag of 0, so this is a bit of a shortcut.
          final = NFA(Tag 0 undefined)

-- Type wrapper allowing equality checks on (infinite) structures based on an id value.
data Tag a = Tag {tag :: Int, unTag :: a} deriving Show
instance Eq (Tag a) where
    (Tag id  _) == (Tag id' _) = id == id'
instance Ord (Tag a) where
    compare (Tag id _) (Tag id' _) = compare id id'
    
data NFA = NFA  (Tag NFAState) deriving (Eq, Ord)
unwrapNFA (NFA (Tag _ nfaState)) = nfaState
getId     (NFA (Tag id _))       = id

instance Show NFA where
    show nfa = "{\n" ++ (showNFA nfa) ++ "\n}"

-- Abbreviation for a Set of NFAs.    
type NfaSet = Set.Set NFA

data NFAState = BlankState NfaSet          -- Set of lambda transitions
              | MatchState Alphabet NfaSet -- Set of transitions if a character is being matched
              | FinalState                 -- Final state. Other states are effectively final by having this as an output. For example, in the regex a*, we typically think of the node matching 'a' as being final, but this representation uses a lambda transition to the (single) FinalState instead.          

-- Advances each of the NFA states by following edges corresponding to the Alphabet argument. 
advance :: NfaSet -> Alphabet -> NfaSet
advance nfas ch = travel advancedStates
    where advancedStates :: NfaSet
          advancedStates = unionMap advanceState nfas
          advanceState :: NFA -> NfaSet
          advanceState nfa = case unwrapNFA nfa of
                              (BlankState _)      -> undefined -- TODO: Technically this should not happen, but I'll need to add error checking later.
                              (MatchState c nfas) -> if c == ch then nfas else Set.empty
                              FinalState          -> Set.empty

-- Newtype wrapper for NFAs that have been visited.
newtype VisitedNFAs = V { unwrapVisited :: IntSet.IntSet}
noneVisited = V IntSet.empty
                              
-- State type used to track visited states and producing an NfaSet.
type TrackingState = State VisitedNFAs NfaSet   

-- An if statement that allows its conditional to be a monadic computation.
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM c t f = do cond <- c
               if cond then t else f

hasVisited :: NFA -> State VisitedNFAs Bool
hasVisited nfa = do (V set) <- get
                    return $ (getId nfa) `IntSet.member` set
                     
markVisited :: NFA -> State VisitedNFAs ()
markVisited nfa = do (V set) <- get
                     put $ V $ (getId nfa) `IntSet.insert` set
                      
-- Visits an NFA (if it has not already been visited), marks it as visited, and executes the given function on it, producing a set of successive states.
visit :: NFA -> (NFA -> TrackingState) -> TrackingState
visit nfa f = ifM (hasVisited nfa)
                  (return Set.empty) --then
                  ((markVisited nfa) >> (f nfa)) --else 
-- Travels along lambda edges, ensuring that the returned set contains (only) reachable Matching and Final states. The "only" part of the above should probably be encoded in the type system, but I just want to get this working for now.
travel :: NfaSet -> NfaSet
travel nfas = evalState (recurse nfas) noneVisited
    where recurse :: NfaSet -> TrackingState
          recurse nfas = foldM combine Set.empty (Set.toList nfas)
          combine :: NfaSet -> NFA -> TrackingState
          combine states nfa = do travelled <- travelState nfa
                                  return $ Set.union states travelled
          travelState :: NFA -> TrackingState
          travelState nfa = visit nfa (\nfa -> case unwrapNFA nfa of
                                                (BlankState nfas) -> recurse nfas
                                                _                 -> return $ Set.singleton nfa
                                       )
                                       
-- Converts an NFA to a String representation.
showNFA :: NFA -> String
showNFA nfa = concat $ intersperse "\n" strings
    where strings = map showState $ enumerate nfa
          showState :: NFA -> String
          showState (NFA (Tag id state)) = case state of
                                             (BlankState nfas)   -> "id: " ++ (show id) ++ ", transitions: " ++ (transitions "->" nfas)
                                             (MatchState c nfas) -> "id: " ++ (show id) ++ ", transitions: " ++ (transitions (c:"->") nfas)
                                             FinalState          -> "id: " ++ (show id) ++ ", transitions: Final"
          transitions :: String -> NfaSet -> String
          transitions c nfas = concat $ intersperse ", " $ map (\nfa -> c ++ (show $ getId nfa)) (Set.toList nfas)

-- Produces a list of all states reachable from the given state.
enumerate :: NFA -> [NFA]
enumerate nfa = Set.elems $ evalState (go nfa) noneVisited
    where go :: NFA -> TrackingState
          go nfa = visit nfa (\nfa -> 
                                 case unwrapNFA nfa of
                                  (BlankState nfas)   -> do sets <- forM (Set.elems nfas) go
                                                            return $ foldl' Set.union (Set.singleton nfa) sets
                                  (MatchState _ nfas) -> do sets <- forM (Set.elems nfas) go
                                                            return $ foldl' Set.union (Set.singleton nfa) sets
                                  FinalState          -> return (Set.singleton nfa)
                               )

-- Converts a regular expression to dot format, suitable for rendering by graphviz.                               
toDot :: Regex -> GV.DotGraph Graph.Node
toDot (Regex nfa) = GV.graphToDot params (toGraph nfa)
    where params = GV.nonClusteredParams {
                     GV.globalAttributes = [ GV.GraphAttrs {GV.attrs = [RankDir FromLeft]} ],
                     GV.fmtNode = (\(n, _) -> case n of 
                                                   1 -> [style filled, fillColor Red]
                                                   _ -> []),
                     GV.fmtEdge = \(_, _, el) -> [toLabel el]
                   }
                   
-- Converts an NFA to a Graph.Gr representation
toGraph :: NFA -> Graph.Gr String String 
toGraph nfa = Graph.mkGraph nodes edges
    where states = enumerate nfa
          nodes = map mkNode states
          edges = concatMap mkEdges states
          mkNode (NFA (Tag n _)) = (n, show n)
          mkEdges (NFA (Tag n (BlankState nfas)))   = map (\nfa -> (n, getId nfa, ['ε']))(Set.toList (nfas))
          mkEdges (NFA (Tag n (MatchState c nfas))) = map (\nfa -> (n, getId nfa, [c]))  (Set.toList (nfas))
          mkEdges (NFA (Tag _ FinalState))          = []

-- Matches the given regex against the target, showing each step as a DotGraph         
simulate :: Regex -> MatchTarget -> [GV.DotGraph Graph.Node]
simulate (Regex nfa) s = map dotify intermediates 
    where graph = toGraph nfa
          dotify :: IntSet.IntSet -> GV.DotGraph Graph.Node
          dotify nodes = GV.graphToDot params graph
            where params = GV.nonClusteredParams {
                            GV.globalAttributes = [ GV.GraphAttrs {GV.attrs = [RankDir FromLeft]} ],
                            GV.fmtNode = (\(n, _) -> if IntSet.member n nodes then [style filled, fillColor Red] else []),
                            GV.fmtEdge = \(_, _, el) -> [toLabel el]
                           }
          getNodes :: NfaSet -> IntSet.IntSet
          getNodes nfas = IntSet.fromList $ map getId (Set.toList nfas)
          intermediates :: [IntSet.IntSet]
          intermediates = go (travel $ Set.singleton nfa) s
          go :: NfaSet -> String -> [IntSet.IntSet]
          go nfas []     = [getNodes nfas]
          go nfas (x:xs) = (getNodes nfas):(go (advance nfas x) xs)
          
--TODO: Remove
outputDots :: [GV.DotGraph Graph.Node] -> IO ()
outputDots xs = sequence_ $ map dotify $ zip [0..] xs
    where dotify (n,g) = runGraphviz g Png ("C:\\temp\\graphs\\" ++ (show n) ++ ".png")
