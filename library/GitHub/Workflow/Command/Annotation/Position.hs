module GitHub.Workflow.Command.Annotation.Position
  ( Position (..)
  , Extent (..)
  , HasExtentMaybe (..)
  , HasPositionMaybe (..)
  , SetPosition (..)
  ) where

import Control.Category
import Control.Lens (Lens', lens, re, simple, (?~), (^.))
import Data.Maybe (Maybe (..), maybe)
import GitHub.Workflow.Command.Annotation.Position.Column
import GitHub.Workflow.Command.Annotation.Position.Columns
import GitHub.Workflow.Command.Annotation.Position.Line
import GitHub.Workflow.Command.Isomorphism.Text
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property)

-- | Where an annotation is marked within a file
data Position = Position
  { line :: Line
  , extent :: Maybe Extent
  }

-- | Extra positional data, as a modification to the start 'Line'
data Extent
  = Horizontal Columns
  | EndLine Line

instance AddToProperties Position where
  addToProperties x =
    ((property "line" ?~) . (^. re text) . lineText) x.line
      . maybe id addToProperties x.extent

instance AddToProperties Extent where
  addToProperties = \case
    Horizontal x -> addToProperties x
    EndLine x -> property "endLine" ?~ lineValue x

class HasExtentMaybe a where
  extent :: Lens' a (Maybe Extent)

instance HasExtentMaybe (Maybe Extent) where
  extent = simple

instance HasExtentMaybe Position where
  extent = lens (.extent) \x y -> Position {line = x.line, extent = y}

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

instance FromColumn Extent where
  atColumn = Horizontal . atColumn
