
-- | Utility functions used in the project.
module Regex.Util where

import qualified Data.Set as Set

-- | Utility function for flattening the results of Set.map when f produces more sets.
unionMap :: (Ord a, Ord b) => (a -> Set.Set b) -- ^ Function to map over the elements.
                           -> Set.Set a -- ^ Set to map over.
                           -> Set.Set b -- ^ Result set with the inner sets flattened.
unionMap f s = Set.unions $ map f (Set.toList s)

-- | An if statement that allows its conditional to be a monadic computation.
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM c t f = do cond <- c
               if cond then t else f
