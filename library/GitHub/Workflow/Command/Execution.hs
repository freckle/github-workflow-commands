module GitHub.Workflow.Command.Execution
  ( MonadCommand (..)
  , PrintCommands (..)
  ) where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((.))
import Data.Functor (Functor)
import GitHub.Workflow.Command.Syntax
import System.IO (IO)

class Monad m => MonadCommand m where
  executeCommand :: ToCommand a => a -> m ()

instance MonadCommand IO where
  executeCommand = printByteStringLn . toCommand

newtype PrintCommands m a = PrintCommands (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadCommand (PrintCommands m) where
  executeCommand = liftIO . executeCommand
