
---------------------------
-- Heuristic implemented by 'Map'. Used in A* algorithm
---------------------------
module Algorithms.Heuristic
    ( Heuristic
    -- * Lists
    , fromList

    ) where

import qualified Data.Map as M

-- | A Heuristic with keys @e@ and values @v@
type Heuristic e v = (e -> v)

{---------------------------
  Lists
----------------------------}
-- | Create Heuristic from list
fromList :: (Ord e,Num v,Ord v) => [(e, v)] -> Heuristic e v
fromList l k = M.findWithDefault 0 k (M.fromList l)
