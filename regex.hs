--Tom Boettcher
--April 2011
module Regex (Regex, compile, matches, match) where

import Data.List (foldr1,intersperse)
import qualified Data.Set as Set
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

-- buildNFA :: AST -> NFA
-- buildNFA ast = fst $ recurse ast 1 (Set.singleton final)
    -- where final = NFA (Tag (0, FinalState))
          -- recurse :: AST -> Int -> Set.Set NFA -> (NFA, Int)
          -- recurse Empty         n outs = makeNFA n (BlankState outs)
          -- recurse (Lit c)       n outs = makeNFA n (MatchState c outs)
          -- recurse (Or l r)      n outs = (NFA(Tag(n, BlankState (Set.fromList [left, right]))), n'')
           -- where (left, n')   = recurse l (n+1) outs
                 -- (right, n'') = recurse r n' outs
          -- recurse (Concat l r ) n outs = (left, n'')
           -- where (left, n')   = recurse l n (Set.singleton right)
                 -- (right, n'') = recurse r n' outs
          -- recurse (Star ast)    n outs = (root, n')
           -- where root       = NFA(Tag(n, BlankState (rest `Set.insert` outs))) 
                 -- (rest, n') = recurse ast (n+1) (root `Set.insert` outs)
          -- makeNFA :: Int -> NFAState -> (NFA, Int)
          -- makeNFA n s = (NFA(Tag(n,s)), n+1)
buildNFA ast = evalState (go ast (Set.singleton final)) 1 
    where final = NFA (Tag (0, FinalState))
          go :: AST -> Set.Set NFA -> State Int NFA
          go Empty        outs = makeNFA $ BlankState outs
          go (Lit c)      outs = makeNFA $ MatchState c outs
          go (Or l r)     outs = do left <- go l outs
                                    right <- go r outs
                                    makeNFA $ BlankState (Set.fromList [left, right])
          go (Concat l r) outs = do right <- go r outs
                                    go l (Set.singleton right)
          go (Star ast)   outs = do n <- get
                                    let root = NFA(Tag(n, BlankState(rest `Set.insert` outs)))
                                        (rest, n') = runState (go ast (root `Set.insert` outs)) (n+1)
                                    put n'
                                    return root
          makeNFA :: NFAState -> State Int NFA
          makeNFA state = do n <- get
                             put (n+1)
                             return $ NFA (Tag (n, state))

matches :: Regex -> String -> Bool
matches r s = case match r s of 
                Nothing -> False
                Just _  -> True

--TODO: Matching groups
match :: Regex -> String -> Maybe [String]
match (Regex nfa) s = go (travel $ Set.singleton nfa) s
    where go :: Set.Set NFA -> String -> Maybe [String]
          go nfas []     = if final `Set.member` nfas
                                 then Just []
                                 else Nothing
          go nfas (x:xs) = go (advance nfas x) xs
          --For now, the final state always has a tag of 0, so this is a bit of a shortcut.
          final = NFA(Tag(0,undefined))

type TagId = Int
-- Type wrapper allowing equality checks on (infinite) types based on an id value.
newtype Tag a = Tag (TagId, a) deriving Show
instance Eq (Tag a) where
    (Tag (id, _)) == (Tag (id', _)) = id == id'
instance Ord (Tag a) where
    compare (Tag (id, _)) (Tag (id',_)) = compare id id'
liftTag :: (a -> b) -> Tag a -> Tag b
liftTag f (Tag (id, a)) = Tag (id, f a)
    
data NFA = NFA (Tag NFAState) deriving (Eq, Ord)
unwrapNFA (NFA (Tag (_, nfaState))) = nfaState
getId     (NFA (Tag (id, _)))       = id

instance Show NFA where
    show nfa = "{\n" ++ (showNFA nfa) ++ "\n}"

showNFA :: NFA -> String
showNFA nfa = concat $ intersperse "\n" strings
    where strings = map showState $ enumerate nfa
          showState :: NFA -> String
          showState (NFA (Tag (id, state))) = case state of
                                             (BlankState nfas)   -> "id: " ++ (show id) ++ ", transitions: " ++ (transitions "->" nfas)
                                             (MatchState c nfas) -> "id: " ++ (show id) ++ ", transitions: " ++ (transitions (c:"->") nfas)
                                             FinalState          -> "id: " ++ (show id) ++ ", transitions: Final"
          transitions :: String -> Set.Set NFA -> String
          transitions c nfas = concat $ intersperse ", " $ map (\nfa -> c ++ (show $ getId nfa)) (Set.toList nfas)
          
enumerate :: NFA -> [NFA]
enumerate nfa = evalState (recurse nfa) emptyV
    where recurse :: NFA -> State VisitedNFAs [NFA]
          recurse nfa = do visited <- get
                           if nfa `Set.member` (unwrapVisited visited)
                            then return []
                            else do put $ liftVisited (Set.insert nfa) visited
                                    subs <- (case unwrapNFA nfa of 
                                              (BlankState nfas)   -> foldM folder [] (Set.toList nfas)
                                              (MatchState c nfas) -> foldM folder [] (Set.toList nfas)
                                              FinalState          -> return []
                                             )
                                    return $ nfa:subs
          folder :: ([NFA] -> NFA -> State VisitedNFAs [NFA])
          folder acc nfa = do result <- recurse nfa
                              return $ result ++ acc

type Alphabet = Char
data NFAState = BlankState (Set.Set NFA)          -- Set of lambda transitions
              | MatchState Alphabet (Set.Set NFA) -- Set of transitions if a character is being matched
              | FinalState                        -- Final state. Other states are effectively final by having this as an output. For example, in the regex a*, we typically think of the node matching 'a' as being final, but this representation uses a lambda transition to the (single) FinalState instead.
              
newtype VisitedNFAs = V (Set.Set NFA)
liftVisited f (V set) = V $ f set
unwrapVisited (V set) = set
emptyV = V Set.empty


-- Advances each of the NFA states by following edges corresponding to the Alphabet argument. 
advance :: Set.Set NFA -> Alphabet -> Set.Set NFA
advance nfas ch = travel advancedStates
    where advancedStates :: Set.Set NFA
          advancedStates = unionMap advanceState nfas
          advanceState :: NFA -> Set.Set NFA
          advanceState nfa = case unwrapNFA nfa of
                              (BlankState _)      -> Set.empty -- TODO: Technically this should not happen, but I'll need to add error checking later.
                              (MatchState c nfas) -> if c == ch then nfas else Set.empty
                              FinalState          -> Set.empty
    
type TrackingState = State VisitedNFAs (Set.Set NFA)    
-- Travels along lambda edges, ensuring that the returned set contains (only) reachable Matching and Final states. The "only" part of the above should probably be encoded in the type system, but I just want to get this working for now.
travel :: Set.Set NFA -> Set.Set NFA
travel nfas = evalState (recurse nfas) emptyV
    where recurse :: Set.Set NFA -> TrackingState
          recurse nfas = foldM combine Set.empty (Set.toList nfas)
          combine :: Set.Set NFA -> NFA -> TrackingState
          combine states nfa = do travelled <- travelState nfa
                                  return $ Set.union states travelled
          travelState :: NFA -> TrackingState
          travelState nfa = case unwrapNFA nfa of
                             -- (BlankState nfas) -> ifNotVisited travelState nfa
                             (BlankState nfas) -> do visited <- get
                                                     if nfa `Set.member` (unwrapVisited visited)
                                                      then return Set.empty
                                                      else do put $ liftVisited (Set.insert nfa) visited
                                                              recurse nfas
                             _                 -> return $ Set.singleton nfa
                             
testNFA = a
            where a = NFA (Tag (0, BlankState (Set.fromList [a,b,c])))
                  b = NFA (Tag (1, FinalState))
                  c = NFA (Tag (2, MatchState 'c' (Set.fromList [a])))
                  
pathological n = matches (compile $ (recurse n) ++ (take n $ repeat 'a')) (take n $ repeat 'a')
    where recurse 0 = ""
          recurse n = "a?" ++ (recurse (n-1))
