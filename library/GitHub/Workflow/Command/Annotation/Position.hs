module GitHub.Workflow.Command.Annotation.Position
  ( Position (..)
  , Extent (..)
  , HasExtentMaybe (..)
  , Columns (..)
  , HasPositionMaybe (..)
  , SetPosition (..)
  , startColumn
  , endColumn
  ) where

import Control.Category
import Control.Lens (Lens', lens, re, simple, (?~), (^.))
import Control.Lens.TH
import Data.Maybe (Maybe (..), maybe)
import GitHub.Workflow.Command.Annotation.Position.Column
import GitHub.Workflow.Command.Annotation.Position.Line
import GitHub.Workflow.Command.Isomorphism.Text
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property)

-- | Where an annotation is marked within a file
data Position = Position
  { line :: Line
  , extent :: Maybe Extent
  }

data Extent
  = Horizontal Columns
  | EndLine Line

data Columns = Columns
  { start :: Column
  , end :: Maybe Column
  }

makeLensesFor [("start", "startColumn")] ''Columns
makeLensesFor [("end", "endColumn")] ''Columns

instance AddToProperties Position where
  addToProperties x =
    ((property "line" ?~) . (^. re text) . lineText) x.line
      . maybe id addToProperties x.extent

instance AddToProperties Extent where
  addToProperties = \case
    Horizontal x -> addToProperties x
    EndLine x -> property "endLine" ?~ lineValue x

instance AddToProperties Columns where
  addToProperties x =
    (property "col" ?~ columnValue x.start)
      . maybe id (\y -> property "endColumn" ?~ columnValue y) x.end

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

instance FromColumn Columns where
  atColumn x = Columns {start = x, end = Nothing}

instance FromColumn Extent where
  atColumn = Horizontal . atColumn
