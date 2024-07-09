module GitHub.Workflow.Command.Annotation.File
  ( File (..)
  , fileValue
  ) where

import Control.Category
import Data.String (IsString)
import Data.Text (Text)
import GitHub.Workflow.Command.Syntax (Value (..))
import Prelude (Eq, Ord, Show)

newtype File = File {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

fileValue :: File -> Value
fileValue = Value . (.text)
