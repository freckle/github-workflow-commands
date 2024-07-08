module GitHub.Workflow.Command.Annotation.Error
  ( error
  , Error (..)
  ) where

import Control.Category
import Control.Lens (lens)
import GitHub.Workflow.Command.Annotation.GenericAnnotation
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
  deriving (ToCommand, ToByteString) via GenericAnnotation Error

instance IsAnnotationType Error where
  annotationTypeName = "error"

instance HasMessage Error where
  message = lens
    (.message)
    \x y -> Error {properties = x.properties, message = y}

instance HasProperties Error where
  annotationProperties =
    lens
      (.properties)
      \x y -> Error {message = x.message, properties = y}

instance HasLocationMaybe Error where
  location = annotationProperties . location

instance FromMessage Error where
  fromMessage = error

instance GetProperties Error where
  getProperties = (.properties)

error :: Message -> Error
error x = Error {message = x, properties = Properties.empty}
