module GitHub.Workflow.Command.Annotation.Commands.Warning
  ( warning
  , Warning (..)
  ) where

import Control.Category
import Control.Lens (lens)
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

data Warning = Warning
  { message :: Message
  , properties :: Properties
  }
  deriving (ToCommand, ToByteString) via GenericAnnotation Warning

instance IsAnnotationType Warning where
  annotationTypeName = "warning"

instance HasMessage Warning where
  message = lens
    (.message)
    \x y -> Warning {properties = x.properties, message = y}

instance HasProperties Warning where
  annotationProperties =
    lens
      (.properties)
      \x y -> Warning {message = x.message, properties = y}

instance HasLocationMaybe Warning where
  location = annotationProperties . location

instance FromMessage Warning where
  fromMessage = warning

instance GetProperties Warning where
  getProperties = (.properties)

warning :: Message -> Warning
warning x = Warning {message = x, properties = Properties.empty}
