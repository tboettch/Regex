-- | Internal module used for implementing the public facing Regex module. All exported names are subject to arbitrary changes between versions.
module Regex.Internal where -- Exporting everything, consumers should not import this directly

import Data.List (foldl', intersperse)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.Monad.State
import qualified Data.Graph.Inductive as Graph
import Data.GraphViz hiding (parse, toDot)
import Data.GraphViz.Attributes.Complete

-- | Utility function for flattening the results of Set.map when f produces more sets.
unionMap :: (Ord a, Ord b) => (a -> Set.Set b) -- ^ Function to map over the elements.
                           -> Set.Set a -- ^ Set to map over.
                           -> Set.Set b -- ^ Result set with the inner sets flattened.
unionMap f s = Set.unions $ map f (Set.toList s)

-- | An unparsed regular expression.
type RawRegex = String

-- | Type representing the set of characters that can be matched by a Regex.
type Alphabet = Char

-- TODO: Remove show
-- | Regular expression type that can be matched against using 'compile' and 'match' below.
newtype Regex = Regex NFA deriving Show

type MatchTarget = [Alphabet]

-- TODO: Error reporting
-- | Compiles the given String into a 'Regex' that can be matched against.
compile :: RawRegex -> Regex
compile = assemble . parse

-- | Assembles an 'AST' into a ready-to-use 'Regex'
assemble :: AST -> Regex
assemble = (Regex) . buildNFA

-- TODO: Extend to allow marking of matching groups, with indexes.
-- | Abstract syntax tree for parsed regular expressions.
data AST = Empty -- ^ Matches the empty string.
         | Lit Alphabet -- ^ Matches a single character.
         | Star AST -- ^ Matches zero or more occurrences of the subtree.
         | Concat AST AST -- ^ Matches the first tree followed by the second tree.
         | Or AST AST -- ^ Matches either the first or second tree.
         deriving Show

-- TODO: Error reporting
-- | Converts a raw expression into an 'AST'.
parse :: RawRegex -> AST
parse input = case stackParse [] input of
                (ast, []) -> ast

type TokenStack = [AST]
-- TODO: Error reporting
-- TODO: Replace this with something prettier
-- | Parses a regular expression using a stack of tokens.
stackParse :: TokenStack -- ^ Stack of tokens processed so far. 
           -> RawRegex -- ^ Remainder of input string.
           -> (AST, RawRegex) -- ^ Resulting AST and the unparsed remainder of the input string.
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
  
-- | Concatenates the contents of a stack.
concatStack :: [AST] -> AST
concatStack ts = foldr1 Concat (reverse ts)

-- | Builds an 'NFA' from an 'AST'.
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

-- | Matches a regular expression against a string.
matches :: Regex -- ^ The 'Regex' to test with.
        -> MatchTarget -- ^ The string to be matched against.
        -> Bool -- ^ True if the string matches, false if not.
matches r s = case match r s of 
                Nothing -> False
                Just _  -> True

--TODO: Matching groups
-- | Matches a regular expression against a string, producing a list of matched subexpressions.
--
-- NOTE: Matching groups are not yet implemented and the resulting list will always be empty.
match :: Regex -- ^ The 'Regex' to test with.
      -> MatchTarget -- ^ The string to be matched against.
      -> Maybe [MatchTarget] -- ^ 'Just' a list of matched subexpressions (if the string matched), or 'Nothing' if the string did not match.
match (Regex nfa) s = go (travel $ Set.singleton nfa) s
    where go :: NfaSet -> String -> Maybe [String]
          go nfas []     = if final `Set.member` nfas
                                 then Just []
                                 else Nothing
          go nfas (x:xs) = go (advance nfas x) xs
          --For now, the final state always has a tag of 0, so this is a bit of a shortcut.
          final = NFA(Tag 0 undefined)

-- Type wrapper allowing equality checks on (infinite) structures based on an id value.
data Tag a = Tag {tag :: Int -- ^ The tag assigned to this value.
                 , unTag :: a -- ^ The wrapped value.
                 } deriving Show
instance Eq (Tag a) where
    (Tag id  _) == (Tag id' _) = id == id'
instance Ord (Tag a) where
    compare (Tag id _) (Tag id' _) = compare id id'
    
-- | A node of a nondeterminsitic finite acceptor. Wraps an 'NFAState' with a unique label to aid in cycle detection.
-- Note the, for our purposes, the initial state effectively represents the entire NFA as well.
data NFA = NFA  (Tag NFAState) deriving (Eq, Ord)
-- | Gets the underlying 'NFAState' of this NFA
unwrapNFA :: NFA -> NFAState
unwrapNFA (NFA (Tag _ nfaState)) = nfaState
-- | Gets the integer tag identifyin this NFA
getId :: NFA -> Int
getId (NFA (Tag id _)) = id

instance Show NFA where
    show nfa = "{\n" ++ (showNFA nfa) ++ "\n}"

-- | Abbreviation for a Set of NFAs.    
type NfaSet = Set.Set NFA

-- | Type representing behaviors of individual 'NFA' nodes.
data NFAState = BlankState NfaSet          -- ^ Set of transitions which do not require input.
              | MatchState Alphabet NfaSet -- ^ Set of transitions if a specific character can be matched.
              | FinalState                 -- ^ Final state. Other states are effectively final by having this as an output. For example, in the regex a*, we typically think of the node matching 'a' as being final, but this representation uses a lambda transition to the (single) FinalState instead.          

-- | Advances each of the NFA states by following edges corresponding to the Alphabet argument. 
advance :: NfaSet -- ^ The set of nodes which should be advanced.
        -> Alphabet -- ^ The input token being matched.
        -> NfaSet -- ^ The resulting sets, after matching against the current token.
advance nfas ch = travel advancedStates
    where advancedStates :: NfaSet
          advancedStates = unionMap advanceState nfas
          advanceState :: NFA -> NfaSet
          advanceState nfa = case unwrapNFA nfa of
                              (BlankState _)      -> undefined -- TODO: Technically this should not happen, but I'll need to add error checking later.
                              (MatchState c nfas) -> if c == ch then nfas else Set.empty
                              FinalState          -> Set.empty

-- | A set of 'NFA' nodes that have been visited during the current "step." Identifies nodes by their integer tag.
newtype VisitedNFAs = V { unwrapVisited :: IntSet.IntSet}
noneVisited = V IntSet.empty
                              
-- | State type used to track visited states and producing an NfaSet.
type TrackingState = State VisitedNFAs NfaSet   

-- | An if statement that allows its conditional to be a monadic computation.
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM c t f = do cond <- c
               if cond then t else f

-- | Predicate in 'State' determining whether a specific 'NFA' node has been visited.
hasVisited :: NFA -> State VisitedNFAs Bool
hasVisited nfa = do (V set) <- get
                    return $ (getId nfa) `IntSet.member` set

-- | Marks an 'NFA' node as being visited for the current "step."                     
markVisited :: NFA -> State VisitedNFAs ()
markVisited nfa = do (V set) <- get
                     put $ V $ (getId nfa) `IntSet.insert` set
                      
-- | Visits an NFA node (if it has not already been visited), marks it as visited, and executes the provided function on it, producing a set of successive states.
visit :: NFA -- ^ The node to be visited.
      -> (NFA -> TrackingState) -- ^ A (possibly recursive) computation to be performed on the state.
      -> TrackingState -- ^ The result of the computation (with the given node marked as visited), or an empty set if this node had already been visisted.
visit nfa f = ifM (hasVisited nfa)
                  (return Set.empty)
                  ((markVisited nfa) >> (f nfa))
-- | Travels along lambda edges for each element in the set, ensuring that the returned set contains (only) reachable 'MatchingState's and 'FinalState's (i.e. no 'BlankState's). The "only" part of the above should probably be encoded in the type system, but I just want to get this working for now.
travel :: NfaSet -> NfaSet
travel nfas = evalState (recurse nfas) noneVisited
    where recurse :: NfaSet -> TrackingState
          recurse nfas = foldM combine Set.empty (Set.toList nfas)
          combine :: NfaSet -> NFA -> TrackingState
          combine states nfa = do travelled <- travelState nfa
                                  return $ Set.union states travelled
          travelState :: NFA -> TrackingState
          travelState base = visit base (\nfa -> case unwrapNFA nfa of
                                                  (BlankState nfas) -> recurse nfas
                                                  _                 -> return $ Set.singleton nfa
                                      )
                                       
-- | Converts an 'NFA' to a String representation. Intended for debugging.
showNFA :: NFA -> String
showNFA nfa = concat $ intersperse "\n" strings
    where strings = map showState $ enumerate nfa
          showState :: NFA -> String
          showState (NFA (Tag id state)) = case state of
                                             (BlankState nfas)   -> "id: " ++ (show id) ++ ", transitions: " ++ (transitions "->" nfas)
                                             (MatchState c nfas) -> "id: " ++ (show id) ++ ", transitions: " ++ (transitions (c:"->") nfas)
                                             FinalState          -> "id: " ++ (show id) ++ ", transitions: Final"
          transitions :: String -> NfaSet -> String
          transitions c nfas = concat $ intersperse ", " $ map (\nfa' -> c ++ (show $ getId nfa')) (Set.toList nfas)

-- | Produces a list of all states reachable from the given state; used as a supplement to 'showNFA' above, 
enumerate :: NFA -> [NFA]
enumerate base = Set.elems $ evalState (go base) noneVisited
    where go :: NFA -> TrackingState
          go nfa = visit nfa (\nfa' -> 
                                 case unwrapNFA nfa' of
                                  (BlankState nfas)   -> do sets <- forM (Set.elems nfas) go
                                                            return $ foldl' Set.union (Set.singleton nfa') sets
                                  (MatchState _ nfas) -> do sets <- forM (Set.elems nfas) go
                                                            return $ foldl' Set.union (Set.singleton nfa') sets
                                  FinalState          -> return (Set.singleton nfa')
                               )

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

-- | Matches the given 'Regex' against the target, showing each "step" as a 'DotGraph'.
simulate :: Regex -- ^ The regular expression to simulate.
         -> MatchTarget -- ^ The string that should be matched against the expression.
         -> [DotGraph Graph.Node] -- ^ A list of 'DotGraph's representing the "steps" the algorithm took to reach a decision.
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
