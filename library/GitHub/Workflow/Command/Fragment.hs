module GitHub.Workflow.Command.Fragment
  ( Fragment (..)
  , toByteString
  ) where

import Control.Category
import Data.ByteString
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL

class Fragment a where
  toByteStringBuilder :: a -> BSB.Builder

toByteString :: Fragment a => a -> ByteString
toByteString = BSL.toStrict . BSB.toLazyByteString . toByteStringBuilder
