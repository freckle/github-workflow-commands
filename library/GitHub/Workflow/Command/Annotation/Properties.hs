module GitHub.Workflow.Command.Annotation.Properties
  ( Properties (..)
  , HasProperties (..)
  , GetProperties (..)
  , empty
  ) where

import Control.Category
import Control.Lens (Lens', lens, re, simple, (?~), (^.))
import Data.Maybe (Maybe (..), maybe)
import Data.Text (Text)
import GitHub.Workflow.Command.Annotation.Location
import GitHub.Workflow.Command.Syntax (text)
import GitHub.Workflow.Command.Syntax qualified as Syntax

data Properties = Properties
  { title :: Maybe Text
  -- ^ A title for the annotation
  , location :: Maybe Location
  }

instance HasLocationMaybe Properties where
  location = lens
    (.location)
    \x y -> Properties {title = x.title, location = y}

instance Syntax.AddToProperties Properties where
  addToProperties x =
    maybe id setTitle x.title
      . maybe id Syntax.addToProperties x.location

setTitle :: Text -> Syntax.Properties -> Syntax.Properties
setTitle = (Syntax.property "title" ?~) . (^. re text)

class HasProperties a where
  annotationProperties :: Lens' a Properties

instance HasProperties Properties where
  annotationProperties = simple

class GetProperties a where
  getProperties :: a -> Properties

instance GetProperties Properties where
  getProperties = id

empty :: Properties
empty = Properties Nothing Nothing
