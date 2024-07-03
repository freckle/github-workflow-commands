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
  , toByteString
  , toByteStringBuilder
  ) where

import Control.Category
import Data.ByteString
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as Foldable
import Data.Maybe (Maybe)
import Data.String (IsString (fromString))
import GitHub.Workflow.Command.Message (Message)
import GitHub.Workflow.Command.Message qualified as Message
import GitHub.Workflow.Command.Name (Name)
import GitHub.Workflow.Command.Name qualified as Name
import GitHub.Workflow.Command.Properties (Key, Properties, ToValue, Value)
import GitHub.Workflow.Command.Properties qualified as Properties
import Prelude (Eq, Ord, Show, (<>))

data Command = Command
  { name :: Name
  , properties :: Properties
  , message :: Message
  }
  deriving stock (Eq, Ord, Show)

instance IsString Command where
  fromString = fromString @Name >>> command

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

toByteStringBuilder :: Command -> BSB.Builder
toByteStringBuilder x =
  "::"
    <> Name.toByteStringBuilder x.name
    <> Foldable.foldMap @Maybe (" " <>) (Properties.toByteStringBuilder x.properties)
    <> "::"
    <> Message.toByteStringBuilder x.message

toByteString :: Command -> ByteString
toByteString = toByteStringBuilder >>> BSB.toLazyByteString >>> BSL.toStrict
