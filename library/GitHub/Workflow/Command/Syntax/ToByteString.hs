module GitHub.Workflow.Command.Syntax.ToByteString
  ( ToByteString (..)
  ) where

import Control.Category
import Data.ByteString
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL

class ToByteString a where
  toByteStringBuilder :: a -> BSB.Builder

  toByteString :: a -> ByteString
  toByteString = BSL.toStrict . BSB.toLazyByteString . toByteStringBuilder
