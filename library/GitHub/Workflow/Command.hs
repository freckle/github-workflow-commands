module GitHub.Workflow.Command
  ( -- * The Command type
    Command

    -- * Ancillary types
  , Name
  , Properties
  , Key
  , Value
  , Message

    -- * Building commands
  , command
  , message
  , property

    -- * Printing commands
  , Fragment
  , toByteString
  , toByteStringBuilder
  ) where

import Control.Category
import Control.Monad (mfilter)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.String (IsString (fromString))
import GitHub.Workflow.Command.Fragment
import GitHub.Workflow.Command.Message (Message)
import GitHub.Workflow.Command.Name (Name)
import GitHub.Workflow.Command.Properties (Key, Properties, ToValue, Value)
import GitHub.Workflow.Command.Properties qualified as Properties
import Prelude (Eq, Maybe (..), Ord, Show, not, (<>))

data Command = Command
  { name :: Name
  , properties :: Properties
  , message :: Message
  }
  deriving stock (Eq, Ord, Show)

instance IsString Command where
  fromString = command . fromString

command :: Name -> Command
command x =
  Command
    { name = x
    , properties = Properties.empty
    , message = ""
    }

message :: Message -> Command -> Command
message m x = x {message = m}

property :: ToValue v => Key -> v -> Command -> Command
property k v x = x {properties = Properties.set k v x.properties}

instance Fragment Command where
  toByteStringBuilder x =
    "::"
      <> toByteStringBuilder x.name
      <> foldMap @Maybe
        (\p -> " " <> toByteStringBuilder p)
        (mfilter (not . Properties.null) (Just x.properties))
      <> "::"
      <> toByteStringBuilder x.message
