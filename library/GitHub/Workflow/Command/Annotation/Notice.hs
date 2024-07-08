module GitHub.Workflow.Command.Annotation.Notice
  ( notice
  , Notice (..)
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

data Notice = Notice
  { message :: Message
  , properties :: Properties
  }
  deriving (ToCommand, ToByteString) via GenericAnnotation Notice

instance IsAnnotationType Notice where
  annotationTypeName = "notice"

instance HasMessage Notice where
  message = lens
    (.message)
    \x y -> Notice {properties = x.properties, message = y}

instance HasProperties Notice where
  annotationProperties =
    lens
      (.properties)
      \x y -> Notice {message = x.message, properties = y}

instance HasLocationMaybe Notice where
  location = annotationProperties . location

instance GetProperties Notice where
  getProperties = (.properties)

instance FromMessage Notice where
  fromMessage = notice

notice :: Message -> Notice
notice x = Notice {message = x, properties = Properties.empty}
