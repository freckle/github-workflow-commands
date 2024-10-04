-- | Programs run by GitHub Actions can use workflow commands to communicate with the runner.
--
-- GitHub documentation:
-- <https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/workflow-commands-for-github-actions Workflow commands for GitHub Actions>
module GitHub.Workflow.Command
  ( -- * Executing commands
    MonadCommand (..)
  , PrintCommands (..)

    -- * Commands
  , ToCommand (..)

    -- ** Setting a debug message
  , Debug (..)
  , debug

    -- ** Setting a notice message
  , Notice (..)
  , notice

    -- ** Setting a warning message
  , Warning (..)
  , warning

    -- ** Setting an error message
  , Error (..)
  , error

    -- ** Grouping log lines
  , group
  , GroupStart (..)
  , GroupEnd (..)

    -- ** Masking a value in a log
  , AddMask (..)

    -- ** Stopping and starting workflow commands
  , suspendCommands
  , stopCommands
  , resumeCommands
  , SuspendToken

    -- * Location
  , Location (..)
  , HasLocationMaybe (..)

    -- ** File
  , File (..)
  , inFile
  , file

    -- ** Position
  , Position (..)
  , position
  , Extent (..)
  , extent
  , Columns (..)
  , line
  , startColumn
  , endColumn
  , Line (..)
  , atLine
  , Column (..)
  , atColumn

    -- * Anatomy of a command
  , Command

    -- ** Name
  , Name (..)
  , HasName (..)

    -- ** Message
  , Message (..)
  , HasMessage (..)

    -- ** Properties
  , Properties
  , HasProperties (..)
  , Key (..)
  , Value (..)
  ) where

import GitHub.Workflow.Command.Annotation
  ( Column (..)
  , Columns (..)
  , Debug (..)
  , Error (..)
  , Extent (..)
  , File (..)
  , HasLocationMaybe (..)
  , Line (..)
  , Location (..)
  , Notice (..)
  , Position (..)
  , Warning (..)
  , atColumn
  , atLine
  , debug
  , endColumn
  , error
  , extent
  , file
  , inFile
  , line
  , notice
  , position
  , startColumn
  , warning
  )
import GitHub.Workflow.Command.Execution
  ( MonadCommand (..)
  , PrintCommands (..)
  )
import GitHub.Workflow.Command.Grouping
  ( GroupEnd (..)
  , GroupStart (..)
  , group
  )
import GitHub.Workflow.Command.Masking (AddMask (..))
import GitHub.Workflow.Command.Stopping
  ( SuspendToken (..)
  , resumeCommands
  , stopCommands
  , suspendCommands
  )
import GitHub.Workflow.Command.Syntax
  ( Command
  , HasMessage (..)
  , HasName (..)
  , HasProperties (..)
  , Key (..)
  , Message (..)
  , Name (..)
  , Properties
  , ToCommand (..)
  , Value (..)
  )
