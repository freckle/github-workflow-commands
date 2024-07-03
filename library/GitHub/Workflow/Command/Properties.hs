module GitHub.Workflow.Command.Properties
  ( Properties
  , Key
  , Value
  , ToValue
  , empty
  , set
  , toByteStringBuilder
  ) where

import           Control.Category
import           Control.Monad                            (mfilter)
import           Data.Bool                                (not)
import qualified Data.ByteString.Builder                  as BSB
import qualified Data.Foldable                            as Foldable
import           Data.Functor
import qualified Data.List                                as List
import           Data.Map.Strict                          (Map)
import qualified Data.Map.Strict                          as Map
import           Data.Maybe                               (Maybe (..))
import           Data.Semigroup
import           GitHub.Workflow.Command.Properties.Key   (Key)
import qualified GitHub.Workflow.Command.Properties.Key   as Key
import           GitHub.Workflow.Command.Properties.Value (ToValue, Value,
                                                           toValue)
import qualified GitHub.Workflow.Command.Properties.Value as Value
import           Prelude                                  (Eq, Ord, Show)

newtype Properties = Properties {map :: Map Key Value}
  deriving stock (Eq, Ord, Show)

empty :: Properties
empty = Properties Map.empty

set :: ToValue v => Key -> v -> Properties -> Properties
set k v = (.map) >>> Map.insert k (toValue v) >>> Properties

toByteStringBuilder :: Properties -> Maybe BSB.Builder
toByteStringBuilder =
  (.map)
    >>> Just
    >>> mfilter (not . Map.null)
    >>> fmap
      ( Map.toAscList
          >>> fmap
            ( \(key, value) ->
                Key.toByteStringBuilder key <> "=" <> Value.toByteStringBuilder value
            )
          >>> List.intersperse ","
          >>> Foldable.fold
      )
