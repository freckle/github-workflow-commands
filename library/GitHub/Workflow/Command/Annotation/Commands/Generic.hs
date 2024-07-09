module GitHub.Workflow.Command.Annotation.Commands.Generic
  ( GenericAnnotation (..)
  , IsAnnotationType (..)
  ) where

import Control.Category
import Control.Lens (Iso', coerced, over, (.~), (^.))
import GitHub.Workflow.Command.Annotation.Location
import GitHub.Workflow.Command.Annotation.Properties
import GitHub.Workflow.Command.Syntax
  ( ByteStringViaCommand
  , HasMessage
  , Name
  , ToByteString
  , ToCommand
  )
import GitHub.Workflow.Command.Syntax qualified as Syntax

class IsAnnotationType a where
  annotationTypeName :: Name

newtype GenericAnnotation a = GenericAnnotation a

unwrapped :: Iso' (GenericAnnotation a) a
unwrapped = coerced

deriving via
  (ByteStringViaCommand a)
  instance
    ToCommand a => ToByteString (GenericAnnotation a)

instance
  (IsAnnotationType a, HasMessage a, GetProperties a)
  => ToCommand (GenericAnnotation a)
  where
  addToCommand x =
    (Syntax.name .~ annotationTypeName @a)
      . (Syntax.message .~ (x ^. (unwrapped . Syntax.message)))
      . over Syntax.properties (Syntax.addToProperties (getProperties (x ^. unwrapped)))

instance HasProperties a => HasLocationMaybe (GenericAnnotation a) where
  location = unwrapped . annotationProperties . location
