module GitHub.Workflow.Command.Annotation.Position.Line
  ( Line (..)
  , FromLine (..)
  , lineText
  , lineValue
  ) where

import Control.Category
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TL
import GitHub.Workflow.Command.Syntax (Value (..))
import Numeric.Natural
import Prelude (Eq, Num, Ord, Show)

newtype Line = Line {natural :: Natural}
  deriving newtype (Eq, Ord, Show, Num)

class FromLine a where
  atLine :: Line -> a

instance FromLine Line where
  atLine = id

lineText :: Line -> Text
lineText = TL.toStrict . TB.toLazyText . TL.decimal . (.natural)

lineValue :: Line -> Value
lineValue = Value . lineText
