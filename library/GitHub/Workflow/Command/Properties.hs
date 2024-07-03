module GitHub.Workflow.Command.Properties
  ( Properties
  , Key
  , Value
  , ToValue
  , empty
  , set
  , toByteStringBuilder
  ) where

import Control.Category
import Control.Monad (mfilter)
import Data.Bool (not)
import Data.ByteString.Builder qualified as BSB
import Data.Foldable qualified as Foldable
import Data.Functor
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (..))
import Data.Semigroup
import GitHub.Workflow.Command.Properties.Key (Key)
import GitHub.Workflow.Command.Properties.Key qualified as Key
import GitHub.Workflow.Command.Properties.Value (ToValue, Value, toValue)
import GitHub.Workflow.Command.Properties.Value qualified as Value
import Prelude (Eq, Ord, Show)

newtype Properties = Properties {map :: Map Key Value}
  deriving stock (Eq, Ord, Show)

empty :: Properties
empty = Properties Map.empty

set :: ToValue v => Key -> v -> Properties -> Properties
set k v = Properties . Map.insert k (toValue v) . (.map)

toByteStringBuilder :: Properties -> Maybe BSB.Builder
toByteStringBuilder =
  fmap
    ( Foldable.fold
        . List.intersperse ","
        . fmap
          ( \(key, value) ->
              Key.toByteStringBuilder key <> "=" <> Value.toByteStringBuilder value
          )
        . Map.toAscList
    )
    . mfilter (not . Map.null)
    . Just
    . (.map)
