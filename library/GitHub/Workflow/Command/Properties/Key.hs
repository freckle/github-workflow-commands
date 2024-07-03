module GitHub.Workflow.Command.Properties.Key
  ( Key
  , toByteStringBuilder
  ) where

import           Control.Category
import qualified Data.ByteString.Builder as BSB
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as T
import           Prelude                 (Eq, Ord, Show)

newtype Key = Key {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

toByteStringBuilder :: Key -> BSB.Builder
toByteStringBuilder = (.text) >>> T.encodeUtf8Builder
