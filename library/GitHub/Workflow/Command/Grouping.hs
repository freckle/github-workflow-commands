module GitHub.Workflow.Command.Grouping
  ( group
  , GroupStart (..)
  , GroupEnd (..)
  ) where

import Control.Applicative ((*>), (<*))
import Control.Lens ((.~))
import Data.Function ((.))
import Data.Text (Text)
import GitHub.Workflow.Command.Execution
import GitHub.Workflow.Command.Syntax

-- | Creates an expandable group in the log
group
  :: MonadCommand m
  => Text
  -- ^ Group title
  -> m a
  -- ^ Anything printed within this action will be
  --   nested inside an expandable entry in the log
  -> m a
group title x =
  executeCommand GroupStart {title}
    *> x
    <* executeCommand GroupEnd

-- | Command to start a 'group'
newtype GroupStart = GroupStart {title :: Text}

instance ToCommand GroupStart where
  addToCommand GroupStart {title} =
    (name .~ "group") . (message .~ Message title)

data GroupEnd = GroupEnd

instance ToCommand GroupEnd where
  addToCommand GroupEnd = name .~ "endgroup"
