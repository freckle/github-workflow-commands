module GitHub.Workflow.Command
  ( -- * The Command type
    Command

    -- * Ancillary types
  , Name
  , Properties
  , Key
  , Value
  , Message

    -- * Working with commands
  , TextIso (..)
  , command
  , HasMessage (..)
  , property
  , HasProperties (..)

    -- * Printing commands
  , ToByteStringBuilder
  , toByteString
  , toByteStringBuilder
  ) where

import Control.Category
import Control.Lens (lens)
import Control.Monad (mfilter)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.String (IsString (fromString))
import GitHub.Workflow.Command.Message (HasMessage (..), Message)
import GitHub.Workflow.Command.Name (Name)
import GitHub.Workflow.Command.Properties
  ( HasProperties (..)
  , Key
  , Properties
  , Value
  , property
  )
import GitHub.Workflow.Command.Properties qualified as Properties
import GitHub.Workflow.Command.TextIso (TextIso (..))
import GitHub.Workflow.Command.ToByteStringBuilder
import Prelude (Eq, Maybe (..), Ord, Show, not, (<>))

data Command = Command
  { name :: Name
  , properties :: Properties
  , message :: Message
  }
  deriving stock (Eq, Ord, Show)

instance IsString Command where
  fromString = command . fromString

instance HasMessage Command where
  message = lens (.message) \x y -> x {message = y}

instance HasProperties Command where
  properties = lens (.properties) \x y -> x {properties = y}

command :: Name -> Command
command x =
  Command
    { name = x
    , properties = Properties.empty
    , message = ""
    }

instance ToByteStringBuilder Command where
  toByteStringBuilder x =
    "::"
      <> toByteStringBuilder x.name
      <> foldMap @Maybe
        (\p -> " " <> toByteStringBuilder p)
        (mfilter (not . Properties.null) (Just x.properties))
      <> "::"
      <> toByteStringBuilder x.message
