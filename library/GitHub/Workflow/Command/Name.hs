module GitHub.Workflow.Command.Name
  ( Name
  , toByteStringBuilder
  ) where

import           Control.Category
import qualified Data.ByteString.Builder as BSB
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Prelude                 (Eq, Ord, Show)

newtype Name = Name {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

toByteStringBuilder :: Name -> BSB.Builder
toByteStringBuilder =
  (.text)
    >>> (\x -> if T.null x then "missing.command" else x)
    >>> T.encodeUtf8Builder
