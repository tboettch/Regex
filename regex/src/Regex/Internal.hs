-- | Internal module used for implementing the public facing Regex module. All exported names are subject to arbitrary changes between versions.
module Regex.Internal where -- Exporting everything, consumers should not import this directly

import Data.List (foldl', intersperse)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.State.Strict
import qualified Regex.Parser as Parser
import Regex.Parser (AST (Empty, Lit, Or, Concat, Star), RawRegex, Alphabet, regex)
import Regex.Util
import Regex.NFA
import qualified Text.Parsec as Parsec

-- TODO: Remove show
-- | Regular expression type that can be matched against using 'compile' and 'match' below.
newtype Regex = Regex NFA deriving Show

type MatchTarget = [Alphabet]

-- | Compiles the given String into a 'Regex' that can be matched against.
compile :: RawRegex -> Either String Regex
compile str = fmap assemble (Parser.parse str)

-- | Assembles an 'AST' into a ready-to-use 'Regex'
assemble :: AST -> Regex
assemble = (Regex) . buildNFA

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

-- | Advances each of the NFA states by following edges corresponding to the current token. 
advance :: NfaSet -- ^ The set of nodes which should be advanced.
        -> Alphabet -- ^ The input token being matched.
        -> NfaSet -- ^ The resulting nodes, after matching against the current token.
advance nfas ch = travel $ unionMap advanceState nfas
    where advanceState :: NFA -> NfaSet
          advanceState nfa = case unwrapNFA nfa of
                              (BlankState _)      -> undefined -- TODO: Technically this should not happen, but I'll need to add error checking later.
                              (MatchState c nfas) -> if c == ch then nfas else Set.empty
                              FinalState          -> Set.empty

-- | A set of 'NFA' nodes that have been visited during the current "step." Identifies nodes by their integer tag.
newtype VisitedNFAs = V { unwrapVisited :: IntSet.IntSet}
noneVisited = V IntSet.empty
                              
-- | State type used to track visited states and producing an NfaSet.
type TrackingState = State VisitedNFAs NfaSet                       
                      
-- | Visits an NFA node (if it has not already been visited), marks it as visited, and executes the provided function on it, producing a set of successive states.
visit :: NFA -- ^ The node to be visited.
      -> (NFA -> TrackingState) -- ^ A (possibly recursive) computation to be performed on the state.
      -> TrackingState -- ^ The result of the computation (with the given node marked as visited), or an empty set if this node had already been visisted.
visit nfa f = ifM (hasVisited nfa)
                  (return Set.empty)
                  ((markVisited nfa) >> (f nfa))
  where hasVisited :: NFA -> State VisitedNFAs Bool
        hasVisited nfa = do (V set) <- get
                            return $ (getId nfa) `IntSet.member` set
        markVisited :: NFA -> State VisitedNFAs ()
        markVisited nfa = do (V set) <- get
                             put $ V $ (getId nfa) `IntSet.insert` set
-- | Travels along epsilon edges for each element in the set, ensuring that the returned set contains (only) reachable 'MatchingState's and 'FinalState's (i.e. no 'BlankState's). The "only" part of the above should probably be encoded in the type system, but I just want to get this working for now.
travel :: NfaSet -> NfaSet
travel nfas = evalState (go nfas) noneVisited
    where go :: NfaSet -> TrackingState
          go nfas = foldM combine Set.empty (Set.toList nfas)
          combine :: NfaSet -> NFA -> TrackingState
          combine states nfa = Set.union <$> pure states <*> travelState nfa
          travelState :: NFA -> TrackingState
          travelState base = visit base (\nfa -> case unwrapNFA nfa of
                                                  (BlankState nfas) -> go nfas
                                                  _                 -> return $ Set.singleton nfa
                                      )

instance Show NFA where
    show nfa = "{\n" ++ (showNFA nfa) ++ "\n}"
                               
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
          transitions c = concat . (intersperse ", ") . (map (\nfa' -> c ++ (show $ getId nfa'))) . Set.toList

-- | Produces a list of all nodes reachable from the given node.
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
