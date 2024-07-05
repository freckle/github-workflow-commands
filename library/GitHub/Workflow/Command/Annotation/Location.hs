module GitHub.Workflow.Command.Annotation.Location
  ( Location (..)
  , HasLocationMaybe (..)
  , SetLocation (..)
  ) where

import Control.Category
import Control.Lens (Lens', lens, re, simple, (?~), (^.))
import Data.Maybe (Maybe (..), maybe)
import GitHub.Workflow.Command.Annotation.File
import GitHub.Workflow.Command.Annotation.FilePosition
import GitHub.Workflow.Command.Annotation.LineRange
import GitHub.Workflow.Command.Annotation.SingleLinePosition
import GitHub.Workflow.Command.Syntax
  ( AddToProperties (..)
  , Properties
  , property
  , text
  )

data Location = Location
  { file :: File
  -- ^ The path of the file for which the annotation should be created
  , position :: Maybe FilePosition
  }

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

instance HasFilePositionMaybe Location where
  filePosition = lens
    (.position)
    \x y -> Location {file = x.file, position = y}

instance SetSingleLinePosition Location where
  setSingleLinePosition x y =
    Location
      { file = y.file
      , position = Just (fromSingleLinePosition x)
      }

class SetLocation a where
  setLocation :: Location -> a -> a

instance SetLocation Location where
  setLocation x _ = x

instance SetFilePosition Location where
  setFilePosition x y = Location {file = y.file, position = Just x}

instance SetLineRange Location where
  setLineRange x y = Location {file = y.file, position = Just (fromLineRange x)}
