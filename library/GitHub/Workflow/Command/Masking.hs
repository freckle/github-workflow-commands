module GitHub.Workflow.Command.Masking
  ( AddMask (..)
  ) where

import Control.Lens ((.~))
import Data.Function ((.))
import Data.Text (Text)
import GitHub.Workflow.Command.Syntax

-- | Prevents a string or variable from being printed in the log
--
-- GitHub documentation:
-- <https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions#masking-a-value-in-a-log Masking a value in a log>
newtype AddMask = AddMask
  { value :: Text
  -- ^ An environment variable or string
  }

instance ToCommand AddMask where
  addToCommand AddMask {value} =
    (name .~ "add-mask") . (message .~ Message value)
