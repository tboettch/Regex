-- | Internal module for the NFA representation of regular expressions.
module Regex.NFA where

import Regex.Parser
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.Monad.State.Strict

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

-- | Abbreviation for a Set of NFAs.    
type NfaSet = Set.Set NFA

-- | Type representing behaviors of individual 'NFA' nodes.
data NFAState = BlankState NfaSet          -- ^ Set of transitions which do not require input.
              | MatchState Alphabet NfaSet -- ^ Set of transitions if a specific character can be matched.
              | FinalState                 -- ^ Final state. Other states are effectively final by having this as an output. For example, in the regex a*, we typically think of the node matching 'a' as being final, but this representation uses an epsilon transition to the (single) FinalState instead.      
