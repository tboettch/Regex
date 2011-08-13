--Tom Boettcher
--April 2011
--TODO: Remove testNfa
module Regex (Regex, compile, matches, match, testNfa) where

import Data.List (foldr1, foldl', intersperse)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.Monad.State

-- Utility function for flattening the results of Set.map when f produces more sets.
unionMap f s = Set.unions $ map f (Set.toList s)

type RawRegex = String
-- Regular expression type that can be matched against using 
newtype Regex = Regex NFA

-- Compiles the given String into a Regex that can be matched against.
-- TODO: Error reporting
compile :: RawRegex -> Regex
compile = (Regex).buildNFA.parse

-- Abstract syntax tree for parsed regular expressions.
-- TODO: Extend to allow marking of matching groups, with indexes.
data AST = Empty
         | Lit Char
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
stackParse []     (')':as)       = undefined                                   
stackParse ts     (')':as)       = (concatStack ts, as)
stackParse []     ('*':as)       = undefined
stackParse (x:ts) ('*':as)       = stackParse ((Star x):ts) as
stackParse []     ('+':as)       = undefined
stackParse (x:ts) ('+':as)       = stackParse ((Concat x (Star x)):ts) as
stackParse []     ('?':as)       = undefined
stackParse (x:ts) ('?':as)       = stackParse ((Or x Empty):ts) as
stackParse []     ('|':as)       = undefined
stackParse ts     ('|':as)       = case (stackParse ts [], stackParse [] as) of
                                    ((left, []), (right, remainder)) -> stackParse ((Or left right):[]) remainder
stackParse ts     (a:as)         = stackParse ((Lit a):ts) as
stackParse []     []             = (Empty, [])
stackParse ts     []             = (concatStack ts, [])

concatStack ts = foldr1 Concat (reverse ts)

buildNFA :: AST -> NFA
buildNFA ast = evalState (go ast (Set.singleton final)) 1 
    where final = NFA (Tag 0 FinalState)
          go :: AST -> NfaSet -> State Int NFA
          go Empty        outs = makeNFA $ BlankState outs
          go (Lit c)      outs = makeNFA $ MatchState c outs
          go (Or l r)     outs = do left <- go l outs
                                    right <- go r outs
                                    makeNFA $ BlankState (Set.fromList [left, right])
          go (Concat l r) outs = do right <- go r outs
                                    go l (Set.singleton right)
          go (Star ast)   outs = do n <- get
                                    let root = NFA(Tag n (BlankState(rest `Set.insert` outs)))
                                        (rest, n') = runState (go ast (root `Set.insert` outs)) (n+1)
                                    put n'
                                    return root
          makeNFA :: NFAState -> State Int NFA
          makeNFA state = do n <- get
                             put (n+1)
                             return $ NFA (Tag n state)

matches :: Regex -> String -> Bool
matches r s = case match r s of 
                Nothing -> False
                Just _  -> True

--TODO: Matching groups
match :: Regex -> String -> Maybe [String]
match (Regex nfa) s = go (travel $ Set.singleton nfa) s
    where go :: NfaSet -> String -> Maybe [String]
          go nfas []     = if final `Set.member` nfas
                                 then Just []
                                 else Nothing
          go nfas (x:xs) = go (advance nfas x) xs
          --For now, the final state always has a tag of 0, so this is a bit of a shortcut.
          final = NFA(Tag 0 undefined)

-- Type wrapper allowing equality checks on (infinite) types based on an id value.
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
                              
type NfaSet = Set.Set NFA

type Alphabet = Char
data NFAState = BlankState NfaSet          -- Set of lambda transitions
              | MatchState Alphabet NfaSet -- Set of transitions if a character is being matched
              | FinalState                 -- Final state. Other states are effectively final by having this as an output. For example, in the regex a*, we typically think of the node matching 'a' as being final, but this representation uses a lambda transition to the (single) FinalState instead.
              
newtype VisitedNFAs = V { unwrapVisited :: IntSet.IntSet}
liftVisited f (V set) = V $ f set
emptyV = V IntSet.empty
containsNfa (V set) nfa = (getId nfa) `IntSet.member` set
insertNfa nfa (V set) = V $ (getId nfa) `IntSet.insert` set

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
    
type TrackingState = State VisitedNFAs NfaSet    
-- Travels along lambda edges, ensuring that the returned set contains (only) reachable Matching and Final states. The "only" part of the above should probably be encoded in the type system, but I just want to get this working for now.
travel :: NfaSet -> NfaSet
travel nfas = evalState (recurse nfas) emptyV
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
enumerate nfa = Set.elems $ evalState (go nfa) emptyV
    where go :: NFA -> TrackingState
          go nfa = visit nfa (\nfa -> 
                                 case unwrapNFA nfa of
                                  (BlankState nfas)   -> do sets <- forM (Set.elems nfas) go
                                                            return $ foldl' Set.union (Set.singleton nfa) sets
                                  (MatchState c nfas) -> do sets <- forM (Set.elems nfas) go
                                                            return $ foldl' Set.union (Set.singleton nfa) sets
                                  FinalState          -> return (Set.singleton nfa)
                               )
                  
pathological n = matches (compile $ (recurse n) ++ (take n $ repeat 'a')) (take n $ repeat 'a')
    where recurse 0 = ""
          recurse n = "a?" ++ (recurse (n-1))
          
testNfa = a
            where a = NFA (Tag 0 (BlankState (Set.fromList [a,b,c])))
                  b = NFA (Tag 1 FinalState)
                  c = NFA (Tag 2 (MatchState 'c' (Set.fromList [a])))
