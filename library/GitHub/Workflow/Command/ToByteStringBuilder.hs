module GitHub.Workflow.Command.ToByteStringBuilder
  ( ToByteStringBuilder (..)
  , toByteString
  ) where

import Control.Category
import Data.ByteString
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL

class ToByteStringBuilder a where
  toByteStringBuilder :: a -> BSB.Builder

toByteString :: ToByteStringBuilder a => a -> ByteString
toByteString = BSL.toStrict . BSB.toLazyByteString . toByteStringBuilder
