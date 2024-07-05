module GitHub.Workflow.Command.Isomorphism.Natural
  ( NaturalIso (..)
  ) where

import Control.Lens (Iso')
import Numeric.Natural (Natural)

class NaturalIso a where
  natural :: Iso' a Natural
