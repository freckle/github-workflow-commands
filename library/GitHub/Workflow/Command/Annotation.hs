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
  , Message
  , FromMessage (..)

    -- * Properties
  , Properties (..)

    -- ** Location
  , Location (..)
  , HasLocationMaybe (..)

    -- ** File
  , File
  , FromFile (..)
  , HasFile (..)

    -- ** Position
  , Position (..)
  , HasPositionMaybe (..)
  , SetPosition (..)
  , Extent (..)
  , HasExtentMaybe (..)
  , Columns (..)
  , startColumn
  , endColumn

    -- * Line
  , Line
  , FromLine (..)
  , SetLine (..)

    -- ** Column
  , Column
  , FromColumn (..)
  , SetColumn (..)

    -- * Text
  , TextIso (..)

    -- * Natural
  , NaturalIso (..)

    -- * Output
  , ToCommand (..)
  , toCommand
  , ToByteString (..)
  ) where

import GitHub.Workflow.Command.Annotation.Column
import GitHub.Workflow.Command.Annotation.Commands.Debug
import GitHub.Workflow.Command.Annotation.Commands.Error
import GitHub.Workflow.Command.Annotation.Commands.Notice
import GitHub.Workflow.Command.Annotation.Commands.Warning
import GitHub.Workflow.Command.Annotation.File
import GitHub.Workflow.Command.Annotation.Line
import GitHub.Workflow.Command.Annotation.Location
import GitHub.Workflow.Command.Annotation.Position
import GitHub.Workflow.Command.Annotation.Properties
import GitHub.Workflow.Command.Isomorphism.Natural
import GitHub.Workflow.Command.Isomorphism.Text
import GitHub.Workflow.Command.Syntax
  ( FromMessage (..)
  , Message
  , ToByteString (..)
  , ToCommand (..)
  , toCommand
  )
