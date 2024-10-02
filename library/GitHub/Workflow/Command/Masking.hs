module GitHub.Workflow.Command.Masking
  ( AddMask (..)
  ) where

import Control.Lens ((.~))
import Data.Function ((.))
import Data.Text (Text)
import GitHub.Workflow.Command.Syntax

-- | Command to prevent a string or variable from being printed in the log
newtype AddMask = AddMask
  { value :: Text
  -- ^ An environment variable or string
  }

instance ToCommand AddMask where
  addToCommand AddMask {value} =
    (name .~ "add-mask") . (message .~ Message value)
