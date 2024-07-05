module GitHub.Workflow.Command.Syntax
  ( -- * Command
    Command
  , command
  , ToCommand (..)
  , toCommand

    -- * Name
  , Name
  , HasName (..)

    -- * Message
  , Message
  , HasMessage (..)
  , FromMessage (..)

    -- * Properties
  , Properties
  , Key
  , Value
  , property
  , HasProperties (..)
  , AddToProperties (..)

    -- * Text
  , TextIso (..)

    -- * Output
  , ToByteString (..)
  , ByteStringViaCommand (..)
  ) where

import GitHub.Workflow.Command.Isomorphism.Text
import GitHub.Workflow.Command.Syntax.Command
import GitHub.Workflow.Command.Syntax.Key
import GitHub.Workflow.Command.Syntax.Message
import GitHub.Workflow.Command.Syntax.Name
import GitHub.Workflow.Command.Syntax.Properties
import GitHub.Workflow.Command.Syntax.ToByteString
import GitHub.Workflow.Command.Syntax.Value
