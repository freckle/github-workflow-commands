module GitHub.Workflow.Command.Annotation.LineRange
  ( LineRange (..)
  , FromLineRange (..)
  , SetLineRange (..)
  ) where

import Control.Category
import Control.Lens ((?~))
import GitHub.Workflow.Command.Annotation.Line
import GitHub.Workflow.Command.Syntax
  ( AddToProperties (..)
  , Properties
  , property
  )

data LineRange = LineRange
  { start :: Line
  , end :: Line
  }

instance AddToProperties LineRange where
  addToProperties LineRange {start, end} =
    setStartLine start . setEndLine end

setStartLine :: Line -> Properties -> Properties
setStartLine = (property "line" ?~) . lineValue

setEndLine :: Line -> Properties -> Properties
setEndLine = (property "endLine" ?~) . lineValue

class FromLineRange a where
  fromLineRange :: LineRange -> a

instance FromLineRange LineRange where
  fromLineRange = id

class SetLineRange a where
  setLineRange :: LineRange -> a -> a

instance SetLineRange LineRange where
  setLineRange x _ = x
