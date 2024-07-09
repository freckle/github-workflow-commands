module GitHub.Workflow.Command.Annotation.File
  ( File
  , FromFile (..)
  ) where

import Control.Category
import Control.Lens (iso)
import Data.String (IsString)
import Data.Text (Text)
import GitHub.Workflow.Command.Isomorphism.Text
import Prelude (Eq, Ord, Show)

newtype File = File {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance TextIso File where
  text = iso (.text) File

class FromFile a where
  fromFile :: File -> a

instance FromFile File where
  fromFile = id
