
module Regex (Internal.Regex,
              Internal.compile,
              Internal.matches,
              Internal.match,
              GV.toDot,
              GV.simulate,
              GV.outputDots
             ) where
             
import qualified Regex.Internal as Internal
import qualified Regex.Graphviz as GV
