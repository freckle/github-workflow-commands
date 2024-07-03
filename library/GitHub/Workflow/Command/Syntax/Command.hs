module GitHub.Workflow.Command.Syntax.Command
  ( Command
  , command
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
import GitHub.Workflow.Command.Syntax.ToByteStringBuilder
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

instance ToByteStringBuilder Command where
  toByteStringBuilder x =
    "::"
      <> toByteStringBuilder x.name
      <> foldMap @Maybe
        (\p -> " " <> toByteStringBuilder p)
        (mfilter (not . Properties.null) (Just x.properties))
      <> "::"
      <> toByteStringBuilder x.message
