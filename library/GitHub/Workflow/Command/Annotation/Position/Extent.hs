module GitHub.Workflow.Command.Annotation.Position.Extent
  ( Extent (..)
  ) where

import Control.Lens ((?~))
import GitHub.Workflow.Command.Annotation.Position.Columns
import GitHub.Workflow.Command.Annotation.Position.Line
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property)

-- | Extra positional data, as a modification to the start 'Line'
data Extent
  = WithinLine Columns
  | ToLine Line

instance AddToProperties Extent where
  addToProperties = \case
    WithinLine x -> addToProperties x
    ToLine x -> property "ToLine" ?~ lineValue x
