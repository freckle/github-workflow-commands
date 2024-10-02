module GitHub.Workflow.Command.Stopping
  ( -- * Basic usage
    suspendCommands

    -- * Stop and resume
  , stopCommands
  , resumeCommands
  , SuspendToken (..)

    -- * Manual token management
  , randomSuspendToken
  , suspendCommandsWithToken
  , stopCommandsWithToken

    -- * Command types
  , StopCommands (..)
  , ResumeCommands (..)
  ) where

import Control.Applicative ((*>), (<*))
import Control.Lens ((.~))
import Control.Monad.Random.Class (MonadRandom, getRandomRs)
import Data.Function ((.))
import Data.Functor (Functor ((<$)), (<$>))
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import GitHub.Workflow.Command.Execution
import GitHub.Workflow.Command.Syntax

suspendCommands :: (MonadCommand m, MonadRandom m) => m a -> m a
suspendCommands x = do
  token <- randomSuspendToken
  suspendCommandsWithToken token x

suspendCommandsWithToken :: MonadCommand m => SuspendToken -> m a -> m a
suspendCommandsWithToken token x =
  stopCommandsWithToken token *> x <* resumeCommands token

stopCommands :: (MonadCommand m, MonadRandom m) => m SuspendToken
stopCommands = do
  token <- randomSuspendToken
  token <$ stopCommandsWithToken token

stopCommandsWithToken :: MonadCommand m => SuspendToken -> m ()
stopCommandsWithToken token =
  executeCommand StopCommands {token}

resumeCommands :: MonadCommand m => SuspendToken -> m ()
resumeCommands token = executeCommand ResumeCommands {token}

newtype SuspendToken = SuspendToken Text

randomSuspendToken :: MonadRandom m => m SuspendToken
randomSuspendToken = SuspendToken . T.pack . List.take 20 <$> getRandomRs ('a', 'z')

newtype StopCommands = StopCommands
  { token :: SuspendToken
  }

instance ToCommand StopCommands where
  addToCommand StopCommands {token = SuspendToken t} =
    (name .~ "stop-commands") . (message .~ Message t)

newtype ResumeCommands = ResumeCommands
  { token :: SuspendToken
  }

instance ToCommand ResumeCommands where
  addToCommand ResumeCommands {token = SuspendToken t} =
    name .~ Name t
