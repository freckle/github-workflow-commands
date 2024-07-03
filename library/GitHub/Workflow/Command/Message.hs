module GitHub.Workflow.Command.Message
  ( Message
  , toByteStringBuilder
  ) where

import           Control.Category
import qualified Data.ByteString.Builder as BSB
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Prelude                 (Eq, Ord, Show)

newtype Message = Message {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

toByteStringBuilder :: Message -> BSB.Builder
toByteStringBuilder =
  (.text)
    >>> T.concatMap
      ( \case
          '%'  -> "%25"
          '\r' -> "%0D"
          '\n' -> "%0A"
          x    -> T.singleton x
      )
    >>> T.encodeUtf8Builder
