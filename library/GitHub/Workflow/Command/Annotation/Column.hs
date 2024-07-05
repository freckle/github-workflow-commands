module GitHub.Workflow.Command.Annotation.Column
  ( Column
  , FromColumn (..)
  , HasColumn (..)
  , SetColumn (..)
  , columnText
  , columnValue
  ) where

import Control.Category
import Control.Lens (Lens', iso, re, simple, (^.))
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TL
import GitHub.Workflow.Command.Isomorphism.Natural
import GitHub.Workflow.Command.Isomorphism.Text
import GitHub.Workflow.Command.Syntax (Value)
import Numeric.Natural
import Prelude (Eq, Num, Ord, Show)

newtype Column = Column {natural :: Natural}
  deriving newtype (Eq, Ord, Show, Num)

instance NaturalIso Column where
  natural = iso (.natural) Column

class FromColumn a where
  fromColumn :: Column -> a

instance FromColumn Column where
  fromColumn = id

class HasColumn a where
  column :: Lens' a Column

instance HasColumn Column where
  column = simple

class SetColumn a where
  setColumn :: Column -> a -> a

instance SetColumn Column where
  setColumn x _ = x

columnText :: Column -> Text
columnText = TL.toStrict . TB.toLazyText . TL.decimal . (^. natural)

columnValue :: Column -> Value
columnValue = (^. re text) . columnText
