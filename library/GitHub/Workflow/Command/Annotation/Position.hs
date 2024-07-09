module GitHub.Workflow.Command.Annotation.Position
  ( Position (..)
  , line
  , extent
  ) where

import Control.Category
import Control.Lens ((?~))
import Control.Lens.TH
import Data.Maybe (Maybe (..), maybe)
import GitHub.Workflow.Command.Annotation.Position.Extent
import GitHub.Workflow.Command.Annotation.Position.Line
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property)

-- | Where an annotation is marked within a file
data Position = Position
  { line :: Line
  , extent :: Maybe Extent
  }

makeLensesFor
  [ ("line", "line")
  , ("extent", "extent")
  ]
  ''Position

instance AddToProperties Position where
  addToProperties x =
    (property "line" ?~ lineValue x.line)
      . maybe id addToProperties x.extent

instance FromLine Position where
  atLine x = Position {line = x, extent = Nothing}
