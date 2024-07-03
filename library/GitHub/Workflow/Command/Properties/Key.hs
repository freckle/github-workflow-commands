module GitHub.Workflow.Command.Properties.Key
  ( Key
  ) where

import Control.Category
import Control.Lens (iso)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.TextIso
import GitHub.Workflow.Command.ToByteStringBuilder
import Prelude (Eq, Ord, Show)

newtype Key = Key {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance ToByteStringBuilder Key where
  toByteStringBuilder = T.encodeUtf8Builder . (.text)

instance TextIso Key where
  text = iso (.text) Key
