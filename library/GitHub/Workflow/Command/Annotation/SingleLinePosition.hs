module GitHub.Workflow.Command.Annotation.SingleLinePosition
  ( SingleLinePosition (..)
  , FromSingleLinePosition (..)
  , SetSingleLinePosition (..)
  ) where

import Control.Category
import Control.Lens (lens, re, (.~), (?~), (^.))
import Data.Maybe (Maybe (..), maybe)
import GitHub.Workflow.Command.Annotation.ColumnPosition
import GitHub.Workflow.Command.Annotation.ColumnRange
import GitHub.Workflow.Command.Annotation.Line
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property, text)

data SingleLinePosition = SingleLinePosition
  { line :: Line
  , column :: Maybe ColumnPosition
  }

instance HasLine SingleLinePosition where
  line = lens
    (.line)
    \x y -> SingleLinePosition {column = x.column, line = y}

instance AddToProperties SingleLinePosition where
  addToProperties x =
    ((property "line" ?~) . (^. re text) . lineText) x.line
      . maybe id addToProperties x.column
instance FromLine SingleLinePosition where
  fromLine x = fromSingleLinePosition SingleLinePosition {line = x, column = Nothing}

class FromSingleLinePosition a where
  fromSingleLinePosition :: SingleLinePosition -> a

instance FromSingleLinePosition SingleLinePosition where
  fromSingleLinePosition = id

instance HasColumnPositionMaybe SingleLinePosition where
  columnPosition = lens
    (.column)
    \x y -> SingleLinePosition {line = x.line, column = y}

instance SetColumnRange SingleLinePosition where
  setColumnRange x = columnPosition ?~ fromColumnRange x

class SetSingleLinePosition a where
  setSingleLinePosition :: SingleLinePosition -> a -> a

instance SetSingleLinePosition SingleLinePosition where
  setSingleLinePosition x _ = x

instance SetLine SingleLinePosition where
  setLine = (line .~)
