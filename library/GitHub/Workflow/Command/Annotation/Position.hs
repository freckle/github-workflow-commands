module GitHub.Workflow.Command.Annotation.Position
  ( Position (..)
  , HasPositionMaybe (..)
  , SetPosition (..)
  ) where

import Control.Category
import Control.Lens (Lens', simple)
import Data.Maybe (Maybe)
import GitHub.Workflow.Command.Annotation.Line
import GitHub.Workflow.Command.Annotation.LineRange
import GitHub.Workflow.Command.Annotation.SingleLinePosition
import GitHub.Workflow.Command.Syntax (AddToProperties (..))

-- | Where an annotation is marked within a file
data Position
  = SingleLine SingleLinePosition
  | MultiLine LineRange

instance AddToProperties Position where
  addToProperties = \case
    SingleLine x -> addToProperties x
    MultiLine range -> addToProperties range

class HasPositionMaybe a where
  position :: Lens' a (Maybe Position)

instance HasPositionMaybe (Maybe Position) where
  position = simple

instance FromSingleLinePosition Position where
  fromSingleLinePosition = SingleLine

instance FromLineRange Position where
  fromLineRange = MultiLine

class SetPosition a where
  setPosition :: Position -> a -> a

instance SetPosition Position where
  setPosition x _ = x

instance FromLine Position where
  fromLine = SingleLine . fromLine
