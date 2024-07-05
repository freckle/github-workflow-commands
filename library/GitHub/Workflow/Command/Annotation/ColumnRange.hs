module GitHub.Workflow.Command.Annotation.ColumnRange
  ( ColumnRange (..)
  , HasColumnRangeMaybe (..)
  , FromColumnRange (..)
  , SetColumnRange (..)
  ) where

import Control.Category
import Control.Lens (Lens', simple, (?~))
import Data.Maybe (Maybe)
import GitHub.Workflow.Command.Annotation.Column
import GitHub.Workflow.Command.Syntax
  ( AddToProperties (..)
  , Properties
  , property
  )

data ColumnRange = ColumnRange
  { start :: Column
  , end :: Column
  }

instance AddToProperties ColumnRange where
  addToProperties ColumnRange {start, end} =
    setStartColumn start . setEndColumn end

setStartColumn :: Column -> Properties -> Properties
setStartColumn = (property "col" ?~) . columnValue

setEndColumn :: Column -> Properties -> Properties
setEndColumn = (property "endColumn" ?~) . columnValue

class HasColumnRangeMaybe a where
  columnRange :: Lens' a (Maybe ColumnRange)

instance HasColumnRangeMaybe (Maybe ColumnRange) where
  columnRange = simple

class FromColumnRange a where
  fromColumnRange :: ColumnRange -> a

instance FromColumnRange ColumnRange where
  fromColumnRange = id

class SetColumnRange a where
  setColumnRange :: ColumnRange -> a -> a

instance SetColumnRange ColumnRange where
  setColumnRange x _ = x
