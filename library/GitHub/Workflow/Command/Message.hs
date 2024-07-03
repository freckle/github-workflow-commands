module GitHub.Workflow.Command.Message
  ( Message
  ) where

import Control.Category
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.Fragment
import Prelude (Eq, Ord, Show)

newtype Message = Message {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance Fragment Message where
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
