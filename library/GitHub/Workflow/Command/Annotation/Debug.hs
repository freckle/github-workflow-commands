module GitHub.Workflow.Command.Annotation.Debug
  ( debug
  , Debug (..)
  ) where

import Control.Lens (coerced)
import GitHub.Workflow.Command.Annotation.GenericAnnotation
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
  deriving (ToCommand, ToByteString) via GenericAnnotation Debug

instance IsAnnotationType Debug where
  annotationTypeName = "debug"

instance HasMessage Debug where
  message = coerced

instance FromMessage Debug where
  fromMessage = debug

instance GetProperties Debug where
  getProperties _ = Properties.empty

debug :: Message -> Debug
debug x = Debug {message = x}
