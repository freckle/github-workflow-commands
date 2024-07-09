module GitHub.Workflow.Command.Annotation
  ( -- * Annotations

    -- ** Debug
    debug
  , Debug (..)

    -- ** Error
  , error
  , Error (..)

    -- ** Warning
  , warning
  , Warning (..)

    -- ** Notice
  , notice
  , Notice (..)

    -- * Message
  , Message (..)
  , FromMessage (..)

    -- * Properties
  , Properties (..)

    -- ** Location
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
  , Columns (..)
  , line
  , extent
  , startColumn
  , endColumn

    -- * Line
  , Line (..)
  , FromLine (..)

    -- ** Column
  , atColumn
  , Column (..)

    -- * Output
  , ToCommand (..)
  , toCommand
  , ToByteString (..)
  ) where

import GitHub.Workflow.Command.Annotation.Commands.Debug
import GitHub.Workflow.Command.Annotation.Commands.Error
import GitHub.Workflow.Command.Annotation.Commands.Notice
import GitHub.Workflow.Command.Annotation.Commands.Warning
import GitHub.Workflow.Command.Annotation.File
import GitHub.Workflow.Command.Annotation.Location
import GitHub.Workflow.Command.Annotation.Position
import GitHub.Workflow.Command.Annotation.Position.Column
import GitHub.Workflow.Command.Annotation.Position.Columns
import GitHub.Workflow.Command.Annotation.Position.Extent
import GitHub.Workflow.Command.Annotation.Position.Line
import GitHub.Workflow.Command.Annotation.Properties
import GitHub.Workflow.Command.Syntax
  ( FromMessage (..)
  , Message (..)
  , ToByteString (..)
  , ToCommand (..)
  , toCommand
  )
