module GitHub.Workflow.Command.Annotation.Position.Column
  ( Column (..)
  , columnText
  , columnValue
  ) where

import Control.Category
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TL
import GitHub.Workflow.Command.Syntax (Value (..))
import Numeric.Natural
import Prelude (Eq, Num, Ord, Show)

newtype Column = Column {natural :: Natural}
  deriving newtype (Eq, Ord, Show, Num)

columnText :: Column -> Text
columnText = TL.toStrict . TB.toLazyText . TL.decimal . (.natural)

columnValue :: Column -> Value
columnValue = Value . columnText
