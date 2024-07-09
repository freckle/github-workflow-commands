module GitHub.Workflow.Command.Annotation.Commands.Debug
  ( debug
  , Debug (..)
  ) where

import Control.Lens.TH
import GitHub.Workflow.Command.Annotation.Commands.Generic
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

newtype Debug = Debug
  { message :: Message
  }

makeLensesFor
  [ ("message", "debugMessage")
  ]
  ''Debug

deriving via GenericAnnotation Debug instance ToCommand Debug

deriving via GenericAnnotation Debug instance ToByteString Debug

instance IsAnnotationType Debug where
  annotationTypeName = "debug"

instance HasMessage Debug where
  message = debugMessage

instance FromMessage Debug where
  fromMessage = debug

instance GetProperties Debug where
  getProperties _ = Properties.empty

debug :: Message -> Debug
debug x = Debug {message = x}
