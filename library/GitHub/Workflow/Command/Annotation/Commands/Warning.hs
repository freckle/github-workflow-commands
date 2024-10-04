module GitHub.Workflow.Command.Annotation.Commands.Warning
  ( warning
  , Warning (..)
  ) where

import Control.Category
import Control.Lens.TH
import GitHub.Workflow.Command.Annotation.Commands.Generic
import GitHub.Workflow.Command.Annotation.Location
import GitHub.Workflow.Command.Annotation.Properties
import GitHub.Workflow.Command.Annotation.Properties qualified as Properties
import GitHub.Workflow.Command.Syntax
  ( FromMessage
  , HasMessage
  , Message
  , ToByteString
  , ToCommand
  )
import GitHub.Workflow.Command.Syntax qualified as Syntax

-- | Creates a warning message and prints the message to the log
--
-- The message can be associated with a particular file in your repository,
-- and optionally also a position within the file. See 'HasLocationMaybe'.
--
-- GitHub documentation:
-- <https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions#setting-a-warning-message Setting a warning message>
data Warning = Warning
  { message :: Message
  , properties :: Properties
  }

makeLensesFor
  [ ("message", "warningMessage")
  , ("properties", "warningProperties")
  ]
  ''Warning

deriving via GenericAnnotation Warning instance ToCommand Warning

deriving via GenericAnnotation Warning instance ToByteString Warning

instance IsAnnotationType Warning where
  annotationTypeName = "warning"

instance HasMessage Warning where
  message = warningMessage

instance HasProperties Warning where
  annotationProperties = warningProperties

instance HasLocationMaybe Warning where
  location = annotationProperties . location

instance FromMessage Warning where
  fromMessage = warning

instance GetProperties Warning where
  getProperties = (.properties)

warning :: Message -> Warning
warning x = Warning {message = x, properties = Properties.empty}
