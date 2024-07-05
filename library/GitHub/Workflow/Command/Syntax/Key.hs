module GitHub.Workflow.Command.Syntax.Key
  ( Key
  ) where

import Control.Category
import Control.Lens (iso)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.Isomorphism.Text
import GitHub.Workflow.Command.Syntax.ToByteString
import Prelude (Eq, Ord, Show)

newtype Key = Key {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance ToByteString Key where
  toByteStringBuilder = T.encodeUtf8Builder . (.text)

instance TextIso Key where
  text = iso (.text) Key
