module GitHub.Workflow.Command.Syntax.Command
  ( Command
  , command
  , ToCommand (..)
  , toCommand
  , ByteStringViaCommand (..)
  ) where

import Control.Category
import Control.Lens (lens)
import Control.Monad (mfilter)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.String (IsString (fromString))
import GitHub.Workflow.Command.Syntax.Message
import GitHub.Workflow.Command.Syntax.Name
import GitHub.Workflow.Command.Syntax.Properties
import GitHub.Workflow.Command.Syntax.Properties qualified as Properties
import GitHub.Workflow.Command.Syntax.ToByteString
import Prelude (Eq, Maybe (..), Ord, Show, not, (<>))

-- | A GitHub workflow command
--
-- A 'Command' consists of:
--
-- * 'Name' ('HasName')
-- * 'Message' ('HasMessage')
-- * 'Properties' ('HasProperties')
--
-- Of these, only 'Name' is always required. Some particular types of command require
-- a message or have restrictions on what properties they support or require.
data Command = Command
  { name :: Name
  , message :: Message
  , properties :: Properties
  }
  deriving stock (Eq, Ord, Show)

instance IsString Command where
  fromString = command . fromString

instance HasName Command where
  name = lens
    (.name)
    \x y -> x {name = y}

instance HasMessage Command where
  message = lens
    (.message)
    \x y -> x {message = y}

instance HasProperties Command where
  properties = lens
    (.properties)
    \x y -> x {properties = y}

-- | Construct a minimal command with a command 'Name' e.g. "warning" or "error"
--
-- See the 'GitHub.Workflow.Command.Syntax.Properties.property' and
-- 'GitHub.Workflow.Command.Syntax.Message.message' lenses for other
-- information include in a command.
command :: Name -> Command
command x =
  Command
    { name = x
    , properties = Properties.empty
    , message = ""
    }

instance ToByteString Command where
  toByteStringBuilder x =
    "::"
      <> toByteStringBuilder x.name
      <> foldMap @Maybe
        (\p -> " " <> toByteStringBuilder p)
        (mfilter (not . Properties.null) (Just x.properties))
      <> "::"
      <> toByteStringBuilder x.message

class ToCommand a where
  addToCommand :: a -> Command -> Command

toCommand :: ToCommand a => a -> Command
toCommand x = addToCommand x (command "")

newtype ByteStringViaCommand a = ByteStringViaCommand a

instance ToCommand a => ToByteString (ByteStringViaCommand a) where
  toByteStringBuilder = toByteStringBuilder . toCommand . (\(ByteStringViaCommand x) -> x)
