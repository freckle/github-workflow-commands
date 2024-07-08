module GitHub.Workflow.Command.Annotation.Commands.Notice
  ( notice
  , Notice (..)
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

data Notice = Notice
  { message :: Message
  , properties :: Properties
  }

makeLensesFor
  [ ("message", "noticeMessage")
  , ("properties", "noticeProperties")
  ]
  ''Notice

deriving via GenericAnnotation Notice instance ToCommand Notice

deriving via GenericAnnotation Notice instance ToByteString Notice

instance IsAnnotationType Notice where
  annotationTypeName = "notice"

instance HasMessage Notice where
  message = noticeMessage

instance HasProperties Notice where
  annotationProperties = noticeProperties

instance HasLocationMaybe Notice where
  location = annotationProperties . location

instance GetProperties Notice where
  getProperties = (.properties)

instance FromMessage Notice where
  fromMessage = notice

notice :: Message -> Notice
notice x = Notice {message = x, properties = Properties.empty}
