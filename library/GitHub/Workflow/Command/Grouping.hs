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
--
-- GitHub documentation:
-- <https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions#grouping-log-lines Grouping log lines>
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

-- | Starts a 'group'
newtype GroupStart = GroupStart {title :: Text}

instance ToCommand GroupStart where
  addToCommand GroupStart {title} =
    (name .~ "group") . (message .~ Message title)

-- | Ends a 'group'
data GroupEnd = GroupEnd

instance ToCommand GroupEnd where
  addToCommand GroupEnd = name .~ "endgroup"
