module GitHub.Workflow.CommandSpec
  ( spec
  ) where

import Data.Function
import GitHub.Workflow.Command (message, property)
import GitHub.Workflow.Command qualified as Command
import Test.Hspec

spec :: Spec
spec =
  context "Command.toByteString" $
    do
      specify "error" $
        shouldBe
          ( Command.toByteString $
              "error"
                & message "Missing semicolon"
                & property "file" "app.js"
                & property "line" "1"
          )
          "::error file=app.js,line=1::Missing semicolon"

      specify "debug" $
        shouldBe
          ( Command.toByteString $
              "debug"
                & message "Set the Octocat variable"
          )
          "::debug::Set the Octocat variable"
