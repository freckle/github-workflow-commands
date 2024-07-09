module GitHub.Workflow.Command.Syntax.Name
  ( Name (..)
  , HasName (..)
  ) where

import Control.Category
import Control.Lens (Lens', simple)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.Syntax.ToByteString
import Prelude (Eq, Ord, Show)

newtype Name = Name {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance ToByteString Name where
  toByteStringBuilder =
    T.encodeUtf8Builder
      . (\x -> if T.null x then "missing.command" else x)
      . (.text)

class HasName a where
  name :: Lens' a Name

instance HasName Name where
  name = simple
