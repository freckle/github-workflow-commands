module GitHub.Workflow.Command.Annotation.Location
  ( Location (..)
  , HasLocationMaybe (..)
  , file
  , position
  , inFile
  ) where

import Control.Category
import Control.Lens (Lens', simple, (?~))
import Control.Lens.TH
import Data.Maybe (Maybe (..), maybe)
import Data.String (IsString (..))
import GitHub.Workflow.Command.Annotation.File
import GitHub.Workflow.Command.Annotation.Position
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property)

data Location = Location
  { file :: File
  -- ^ The path of the file for which the annotation should be created
  , position :: Maybe Position
  }

makeLensesFor
  [ ("file", "file")
  , ("position", "position")
  ]
  ''Location

instance IsString Location where
  fromString = inFile . fromString

instance AddToProperties Location where
  addToProperties x =
    (property "file" ?~ fileValue x.file)
      . maybe id addToProperties x.position

inFile :: File -> Location
inFile x = Location {file = x, position = Nothing}

class HasLocationMaybe a where
  location :: Lens' a (Maybe Location)

instance HasLocationMaybe (Maybe Location) where
  location = simple
