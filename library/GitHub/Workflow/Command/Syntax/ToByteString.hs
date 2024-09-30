module GitHub.Workflow.Command.Syntax.ToByteString
  ( ToByteString (..)
  , printByteStringLn
  ) where

import Control.Category
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Semigroup ((<>))
import System.IO (stdout)

class ToByteString a where
  toByteStringBuilder :: a -> BSB.Builder

  toByteString :: a -> ByteString
  toByteString = BSL.toStrict . BSB.toLazyByteString . toByteStringBuilder

printByteStringLn :: (ToByteString a, MonadIO m) => a -> m ()
printByteStringLn =
  liftIO
    . BSB.hPutBuilder stdout
    . (<> BSB.char7 '\n')
    . toByteStringBuilder
