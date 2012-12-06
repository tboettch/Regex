-- | Internal module used for implementing the public facing Regex module. All exported names are subject to arbitrary changes between versions.
module Regex.Internal where -- Exporting everything, consumers should not import this directly

import Data.List (foldl', intersperse)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.Monad.State.Strict
import qualified Regex.Parser as Parser
import Regex.Parser (AST (Empty, Lit, Or, Concat, Star), RawRegex, Alphabet, regex)
import Regex.Util
import qualified Text.Parsec as Parsec

-- TODO: Remove show
-- | Regular expression type that can be matched against using 'compile' and 'match' below.
newtype Regex = Regex NFA deriving Show

type MatchTarget = [Alphabet]

-- TODO: Error reporting
-- | Compiles the given String into a 'Regex' that can be matched against.
compile :: RawRegex -> Regex
compile = assemble . Parser.parse

-- | Assembles an 'AST' into a ready-to-use 'Regex'
assemble :: AST -> Regex
assemble = (Regex) . buildNFA

-- | Builds an 'NFA' from an 'AST'.
buildNFA :: AST -> NFA
buildNFA ast = evalState (go ast (Set.singleton final)) 1 
    where final = NFA (Tag 0 FinalState)
          -- | Recursive helper. Uses 'State' to generate unique labels for each node.
          go :: AST -- ^ The current subtree being evaluated. 
             -> NfaSet -- ^ The set of output states from this subgraph. The 'exterior' edges of this subgraph should point to these nodes.
             -> State Int NFA -- ^ The assembled NFA, with state of the next fresh node label.
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
          -- | Adds a unique label to an 'NFAState' to construct an 'NFA', incrementing the label tracked by 'State'.
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

-- | Wrapper to allow labeling of cyclic graphs. Allows equality checks based on an id value.
data Tag a = Tag {tag :: !Int -- ^ The tag assigned to this value.
                 , unTag :: a -- ^ The wrapped value.
                 } deriving Show
instance Eq (Tag a) where
    (Tag id  _) == (Tag id' _) = id == id'
instance Ord (Tag a) where
    compare (Tag id _) (Tag id' _) = compare id id'
    
-- | A node of a nondeterminsitic finite acceptor. Wraps an 'NFAState' with a unique label to aid in cycle detection.
-- Note that, for our purposes, the initial state effectively represents the entire NFA as well.
data NFA = NFA  !(Tag NFAState) deriving (Eq, Ord)
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
-- | Travels along epsilon edges for each element in the set, ensuring that the returned set contains (only) reachable 'MatchingState's and 'FinalState's (i.e. no 'BlankState's). The "only" part of the above should probably be encoded in the type system, but I just want to get this working for now.
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

-- | Produces a list of all states reachable from the given state.
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
