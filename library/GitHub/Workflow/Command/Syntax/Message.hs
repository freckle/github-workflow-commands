module GitHub.Workflow.Command.Syntax.Message
  ( Message (..)
  , HasMessage (..)
  , FromMessage (..)
  ) where

import Control.Category
import Control.Lens (Lens', simple)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GitHub.Workflow.Command.Syntax.ToByteString
import Prelude (Eq, Ord, Show)

newtype Message = Message {text :: Text}
  deriving newtype (Eq, Ord, Show, IsString)

instance ToByteString Message where
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

class HasMessage a where
  message :: Lens' a Message

instance HasMessage Message where
  message = simple

class FromMessage a where
  fromMessage :: Message -> a

instance FromMessage Message where
  fromMessage = id
