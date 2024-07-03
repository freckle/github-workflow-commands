module GitHub.Workflow.Command.Name
  ( Name
  , toByteStringBuilder
  ) where

import Control.Category
import Data.ByteString.Builder qualified as BSB
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Prelude (Eq, Ord, Show)

newtype Name = Name {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

toByteStringBuilder :: Name -> BSB.Builder
toByteStringBuilder =
  (.text)
    >>> (\x -> if T.null x then "missing.command" else x)
    >>> T.encodeUtf8Builder
