module GitHub.Workflow.Command.Properties
  ( Properties
  , Key
  , Value
  , empty
  , set
  , null
  ) where

import Control.Category
import Data.Foldable (fold)
import Data.Functor
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup
import GitHub.Workflow.Command.Fragment
import GitHub.Workflow.Command.Properties.Key (Key)
import GitHub.Workflow.Command.Properties.Value (Value)
import Prelude (Bool, Eq, Ord, Show)

newtype Properties = Properties {map :: Map Key Value}
  deriving stock (Eq, Ord, Show)

empty :: Properties
empty = Properties Map.empty

set :: Key -> Value -> Properties -> Properties
set k v = Properties . Map.insert k v . (.map)

null :: Properties -> Bool
null = Map.null . (.map)

instance Fragment Properties where
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
