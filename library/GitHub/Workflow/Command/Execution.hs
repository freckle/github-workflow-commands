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

-- | Monadic context in which GitHub workflow commands may be executed
--
-- * For the most basic uses, use the 'IO' instance, which prints commands to 'System.IO.stdout'.
--
-- * For custom monads that support 'MonadIO', you may derive 'MonadCommand' via 'PrintCommands'
--   to get the same behavior that 'IO' exhibits.
--
-- * A program that wishes to accommodate running in both GitHub and non-GitHub contexts
--   may wish to define a more sophisicated 'MonadCommand' instance that prints GitHub
--   workflow commands only when the @GITHUB_ACTIONS@ environment variable is present,
--   and otherwise takes some other more context-appropriate action.
class Monad m => MonadCommand m where
  executeCommand :: ToCommand a => a -> m ()

instance MonadCommand IO where
  executeCommand = printByteStringLn . toCommand

newtype PrintCommands m a = PrintCommands (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadCommand (PrintCommands m) where
  executeCommand = liftIO . executeCommand
