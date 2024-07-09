module GitHub.Workflow.Command.Annotation.Properties
  ( Properties (..)
  , HasProperties (..)
  , GetProperties (..)
  , empty
  ) where

import Control.Category
import Control.Lens (Lens', simple, (?~))
import Control.Lens.TH
import Data.Maybe (Maybe (..), maybe)
import Data.Text (Text)
import GitHub.Workflow.Command.Annotation.Location
import GitHub.Workflow.Command.Syntax (Value (..))
import GitHub.Workflow.Command.Syntax qualified as Syntax

data Properties = Properties
  { title :: Maybe Text
  -- ^ A title for the annotation
  , location :: Maybe Location
  }

makeLensesFor
  [ ("location", "propertiesLocation")
  ]
  ''Properties

instance HasLocationMaybe Properties where
  location = propertiesLocation

instance Syntax.AddToProperties Properties where
  addToProperties x =
    maybe id (\t -> Syntax.property "title" ?~ Value t) x.title
      . maybe id Syntax.addToProperties x.location

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
