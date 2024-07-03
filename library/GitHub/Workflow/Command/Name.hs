module GitHub.Workflow.Command.Name
  ( Name
  ) where

import Control.Category
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.Fragment
import Prelude (Eq, Ord, Show)

newtype Name = Name {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance Fragment Name where
  toByteStringBuilder =
    T.encodeUtf8Builder
      . (\x -> if T.null x then "missing.command" else x)
      . (.text)
