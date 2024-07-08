module GitHub.Workflow.Command.Annotation.Location
  ( Location (..)
  , HasLocationMaybe (..)
  ) where

import Control.Category
import Control.Lens (Lens', lens, re, simple, (?~), (^.))
import Data.Maybe (Maybe (..), maybe)
import Data.String (IsString (..))
import GitHub.Workflow.Command.Annotation.File
import GitHub.Workflow.Command.Annotation.Position
import GitHub.Workflow.Command.Syntax
  ( AddToProperties (..)
  , Properties
  , property
  , text
  )

data Location = Location
  { file :: File
  -- ^ The path of the file for which the annotation should be created
  , position :: Maybe Position
  }

instance IsString Location where
  fromString = fromFile . fromString

instance AddToProperties Location where
  addToProperties x =
    setFile x.file . maybe id addToProperties x.position

setFile :: File -> Properties -> Properties
setFile = (property "file" ?~) . (^. (text . re text))

instance HasFile Location where
  file = lens
    (.file)
    \x y -> Location {position = x.position, file = y}

instance FromFile Location where
  fromFile x = Location {file = x, position = Nothing}

class HasLocationMaybe a where
  location :: Lens' a (Maybe Location)

instance HasLocationMaybe (Maybe Location) where
  location = simple

instance HasPositionMaybe Location where
  position = lens
    (.position)
    \x y -> Location {file = x.file, position = y}

instance SetPosition Location where
  setPosition x y = Location {file = y.file, position = Just x}
