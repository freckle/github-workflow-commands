module GitHub.Workflow.Command.Annotation.FilePosition
  ( FilePosition (..)
  , HasFilePositionMaybe (..)
  , SetFilePosition (..)
  ) where

import Control.Category
import Control.Lens (Lens', simple)
import Data.Maybe (Maybe)
import GitHub.Workflow.Command.Annotation.Line
import GitHub.Workflow.Command.Annotation.LineRange
import GitHub.Workflow.Command.Annotation.SingleLinePosition
import GitHub.Workflow.Command.Syntax (AddToProperties (..))

-- | Where an annotation is marked within a file
data FilePosition
  = SingleLine SingleLinePosition
  | MultiLine LineRange

instance AddToProperties FilePosition where
  addToProperties = \case
    SingleLine x -> addToProperties x
    MultiLine range -> addToProperties range

class HasFilePositionMaybe a where
  filePosition :: Lens' a (Maybe FilePosition)

instance HasFilePositionMaybe (Maybe FilePosition) where
  filePosition = simple

instance FromSingleLinePosition FilePosition where
  fromSingleLinePosition = SingleLine

instance FromLineRange FilePosition where
  fromLineRange = MultiLine

class SetFilePosition a where
  setFilePosition :: FilePosition -> a -> a

instance SetFilePosition FilePosition where
  setFilePosition x _ = x

instance FromLine FilePosition where
  fromLine = SingleLine . fromLine
