module GitHub.Workflow.Command.Message
  ( Message
  , HasMessage (..)
  ) where

import Control.Category
import Control.Lens (Lens', iso, simple)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.TextIso
import GitHub.Workflow.Command.ToByteStringBuilder
import Prelude (Eq, Ord, Show)

newtype Message = Message {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance ToByteStringBuilder Message where
  toByteStringBuilder =
    T.encodeUtf8Builder
      . T.concatMap
        ( \case
            '%' -> "%25"
            '\r' -> "%0D"
            '\n' -> "%0A"
            x -> T.singleton x
        )
      . (.text)

instance TextIso Message where
  text = iso (.text) Message

class HasMessage a where
  message :: Lens' a Message

instance HasMessage Message where
  message = simple
