module GitHub.Workflow.Command.SyntaxSpec
  ( spec
  ) where

import Control.Lens ((.~), (?~))
import Data.Function (($), (&))
import GitHub.Workflow.Command.Syntax
import Test.Hspec

spec :: Spec
spec =
  context "Syntax" do
    specify "empty command name" $
      toByteString (command "") `shouldBe` "::missing.command::"

    specify "endgroup" $
      toByteString (command "endgroup") `shouldBe` "::endgroup::"

    specify "error" $
      toByteString
        ( command "error"
            & message .~ "Missing semicolon"
            & property "file" ?~ "app.js"
            & property "line" ?~ "1"
        )
        `shouldBe` "::error file=app.js,line=1::Missing semicolon"

    specify "debug" $
      toByteString
        ( command "debug"
            & message .~ "Set the Octocat variable"
        )
        `shouldBe` "::debug::Set the Octocat variable"
