module GitHub.Workflow.Command.TextIso
  ( TextIso (..)
  ) where

import Control.Lens (Iso')
import Data.Text (Text)

class TextIso a where
  text :: Iso' a Text
