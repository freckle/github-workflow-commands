module GitHub.Workflow.CommandSpec
  ( spec
  ) where

import Control.Lens
import Data.Function (($))
import Data.Maybe (Maybe (..))
import GitHub.Workflow.Command.Syntax
import Test.Hspec

spec :: Spec
spec =
  context "Command.toByteString" $
    do
      specify "error" $
        shouldBe
          ( toByteString $
              command "error"
                & set message "Missing semicolon"
                & set (property "file") (Just "app.js")
                & set (property "line") (Just "1")
          )
          "::error file=app.js,line=1::Missing semicolon"

      specify "debug" $
        shouldBe
          ( toByteString $
              command "debug"
                & set message "Set the Octocat variable"
          )
          "::debug::Set the Octocat variable"
