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
