module GitHub.Workflow.Command.Annotation.Position.Columns
  ( Columns (..)
  , atColumn
  , startColumn
  , endColumn
  ) where

import Control.Category
import Control.Lens ((?~))
import Control.Lens.TH
import Data.Maybe (Maybe (..), maybe)
import GitHub.Workflow.Command.Annotation.Position.Column
import GitHub.Workflow.Command.Syntax (AddToProperties (..), property)

data Columns = Columns
  { start :: Column
  , end :: Maybe Column
  }

makeLensesFor
  [ ("start", "startColumn")
  , ("end", "endColumn")
  ]
  ''Columns

instance AddToProperties Columns where
  addToProperties x =
    (property "col" ?~ columnValue x.start)
      . maybe id (\y -> property "endColumn" ?~ columnValue y) x.end

atColumn :: Column -> Columns
atColumn x = Columns {start = x, end = Nothing}
