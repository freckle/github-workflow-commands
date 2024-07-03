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

import           Control.Category
import           Data.ByteString
import qualified Data.ByteString.Builder            as BSB
import qualified Data.ByteString.Lazy               as BSL
import qualified Data.Foldable                      as Foldable
import           Data.Maybe                         (Maybe)
import           Data.String                        (IsString (fromString))
import           GitHub.Workflow.Command.Message    (Message)
import qualified GitHub.Workflow.Command.Message    as Message
import           GitHub.Workflow.Command.Name       (Name)
import qualified GitHub.Workflow.Command.Name       as Name
import           GitHub.Workflow.Command.Properties (Key, Properties, ToValue,
                                                     Value)
import qualified GitHub.Workflow.Command.Properties as Properties
import           Prelude                            (Eq, Ord, Show, (<>))

data Command = Command
  { name       :: Name
  , properties :: Properties
  , message    :: Message
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
