module GitHub.Workflow.Command.Annotation.ColumnPosition
  ( ColumnPosition (..)
  , HasColumnPositionMaybe (..)
  ) where

import Control.Category
import Control.Lens (Lens', re, simple, (?~), (^.))
import Data.Maybe (Maybe)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TL
import GitHub.Workflow.Command.Annotation.ColumnRange
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property, text)
import GitHub.Workflow.Command.Syntax.Properties (Properties)
import Numeric.Natural (Natural)

data ColumnPosition
  = SingleColumn Natural
  | MultiColumn ColumnRange

instance AddToProperties ColumnPosition where
  addToProperties = \case
    SingleColumn column -> setStartColumn column
    MultiColumn range -> addToProperties range

setStartColumn :: Natural -> Properties -> Properties
setStartColumn = (property "col" ?~) . (^. re text) . naturalText

naturalText :: Natural -> Text
naturalText = TL.toStrict . TB.toLazyText . TL.decimal

class HasColumnPositionMaybe a where
  columnPosition :: Lens' a (Maybe ColumnPosition)

instance HasColumnPositionMaybe (Maybe ColumnPosition) where
  columnPosition = simple

instance FromColumnRange ColumnPosition where
  fromColumnRange = MultiColumn
