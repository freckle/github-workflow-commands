module GitHub.Workflow.Command.Syntax.Value
  ( Value
  ) where

import Control.Category
import Control.Lens (iso)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.Syntax.TextIso
import GitHub.Workflow.Command.Syntax.ToByteStringBuilder
import Prelude (Eq, Ord, Show)

newtype Value = Value {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance ToByteStringBuilder Value where
  toByteStringBuilder =
    T.encodeUtf8Builder
      . T.concatMap
        ( \case
            '%' -> "%25"
            '\r' -> "%0D"
            '\n' -> "%0A"
            ':' -> "%3A"
            ',' -> "%2C"
            x -> T.singleton x
        )
      . (.text)

instance TextIso Value where
  text = iso (.text) Value
