module GitHub.Workflow.Command.Properties.Key
  ( Key
  ) where

import Control.Category
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.Fragment
import Prelude (Eq, Ord, Show)

newtype Key = Key {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance Fragment Key where
  toByteStringBuilder = T.encodeUtf8Builder . (.text)
