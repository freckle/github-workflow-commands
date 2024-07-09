module GitHub.Workflow.Command.Annotation.Position
  ( Position (..)
  , HasPositionMaybe (..)
  , SetPosition (..)
  , line
  , extent
  ) where

import Control.Category
import Control.Lens (Lens', re, simple, (?~), (^.))
import Control.Lens.TH
import Data.Maybe (Maybe (..), maybe)
import GitHub.Workflow.Command.Annotation.Position.Extent
import GitHub.Workflow.Command.Annotation.Position.Line
import GitHub.Workflow.Command.Isomorphism.Text
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
    ((property "line" ?~) . (^. re text) . lineText) x.line
      . maybe id addToProperties x.extent

class HasPositionMaybe a where
  position :: Lens' a (Maybe Position)

instance HasPositionMaybe (Maybe Position) where
  position = simple

class SetPosition a where
  setPosition :: Position -> a -> a

instance SetPosition Position where
  setPosition x _ = x

instance FromLine Position where
  atLine x = Position {line = x, extent = Nothing}
