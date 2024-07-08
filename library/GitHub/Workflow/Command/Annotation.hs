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

    -- ** File position
  , FilePosition (..)
  , HasFilePositionMaybe (..)
  , SetFilePosition (..)

    -- * Line
  , Line
  , FromLine (..)
  , SetLine (..)

    -- ** Line range
  , LineRange (..)
  , FromLineRange (..)
  , SetLineRange (..)

    -- ** Single-line
  , SingleLinePosition (..)
  , FromSingleLinePosition (..)
  , SetSingleLinePosition (..)

    -- ** Column
  , Column
  , FromColumn (..)
  , SetColumn (..)

    -- ** Column position
  , ColumnPosition (..)
  , HasColumnPositionMaybe (..)

    -- ** Column range
  , ColumnRange (..)
  , HasColumnRangeMaybe (..)
  , FromColumnRange (..)
  , SetColumnRange (..)

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
import GitHub.Workflow.Command.Annotation.ColumnPosition
import GitHub.Workflow.Command.Annotation.ColumnRange
import GitHub.Workflow.Command.Annotation.Debug
import GitHub.Workflow.Command.Annotation.Error
import GitHub.Workflow.Command.Annotation.File
import GitHub.Workflow.Command.Annotation.FilePosition
import GitHub.Workflow.Command.Annotation.Line
import GitHub.Workflow.Command.Annotation.LineRange
import GitHub.Workflow.Command.Annotation.Location
import GitHub.Workflow.Command.Annotation.Notice
import GitHub.Workflow.Command.Annotation.Properties
import GitHub.Workflow.Command.Annotation.SingleLinePosition
import GitHub.Workflow.Command.Annotation.Warning
import GitHub.Workflow.Command.Isomorphism.Natural
import GitHub.Workflow.Command.Isomorphism.Text
import GitHub.Workflow.Command.Syntax
  ( FromMessage (..)
  , Message
  , ToByteString (..)
  , ToCommand (..)
  , toCommand
  )
