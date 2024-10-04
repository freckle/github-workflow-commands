module GitHub.Workflow.Command.Annotation.Commands.Error
  ( error
  , Error (..)
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

-- | Creates an error message and prints the message to the log
--
-- The message can be associated with a particular file in your repository,
-- and optionally also a position within the file. See 'HasLocationMaybe'.
--
-- GitHub documentation:
-- <https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions#setting-an-error-message Setting an error message>
data Error = Error
  { message :: Message
  , properties :: Properties
  }

makeLensesFor
  [ ("message", "errorMessage")
  , ("properties", "errorProperties")
  ]
  ''Error

deriving via GenericAnnotation Error instance ToCommand Error

deriving via GenericAnnotation Error instance ToByteString Error

instance IsAnnotationType Error where
  annotationTypeName = "error"

instance HasMessage Error where
  message = errorMessage

instance HasProperties Error where
  annotationProperties = errorProperties

instance HasLocationMaybe Error where
  location = annotationProperties . location

instance FromMessage Error where
  fromMessage = error

instance GetProperties Error where
  getProperties = (.properties)

error :: Message -> Error
error x = Error {message = x, properties = Properties.empty}
