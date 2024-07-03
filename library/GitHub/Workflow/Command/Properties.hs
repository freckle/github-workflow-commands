module GitHub.Workflow.Command.Properties
  ( Properties
  , HasProperties (..)
  , property
  , Key
  , Value
  , empty
  , set
  , null
  ) where

import Control.Category
import Control.Lens (Lens', at, iso, simple)
import Data.Foldable (fold)
import Data.Functor
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (..))
import Data.Semigroup
import GitHub.Workflow.Command.Properties.Key (Key)
import GitHub.Workflow.Command.Properties.Value (Value)
import GitHub.Workflow.Command.ToByteStringBuilder
import Prelude (Bool, Eq, Ord, Show)

newtype Properties = Properties {map :: Map Key Value}
  deriving stock (Eq, Ord, Show)

empty :: Properties
empty = Properties Map.empty

set :: Key -> Value -> Properties -> Properties
set k v = Properties . Map.insert k v . (.map)

null :: Properties -> Bool
null = Map.null . (.map)

instance ToByteStringBuilder Properties where
  toByteStringBuilder =
    fold
      . List.intersperse ","
      . fmap
        ( \(key, value) ->
            toByteStringBuilder key
              <> "="
              <> toByteStringBuilder value
        )
      . Map.toAscList
      . (.map)

class HasProperties a where
  properties :: Lens' a Properties

instance HasProperties Properties where
  properties = simple

property :: HasProperties a => Key -> Lens' a (Maybe Value)
property k = properties . iso (.map) Properties . at k
