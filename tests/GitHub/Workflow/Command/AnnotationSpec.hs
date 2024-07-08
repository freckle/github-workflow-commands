module GitHub.Workflow.Command.AnnotationSpec
  ( spec
  ) where

import Control.Lens ((?~))
import Data.Function (($), (&))
import GitHub.Workflow.Command.Annotation
import Test.Hspec

spec :: Spec
spec =
  context "Annotation" do
    specify "debug with no properties" $
      toByteString (debug "Set the Octocat variable")
        `shouldBe` "::debug::Set the Octocat variable"

    specify "error with no properties" $
      toByteString (error "Missing semicolon")
        `shouldBe` "::error::Missing semicolon"

    specify "error with file" $
      toByteString (error "Missing semicolon" & location ?~ fromFile "app.js")
        `shouldBe` "::error file=app.js::Missing semicolon"

    specify "error with line number" $
      toByteString
        ( error "Missing semicolon"
            & location ?~ (fromFile "app.js" & setSingleLinePosition (fromLine 1))
        )
        `shouldBe` "::error file=app.js,line=1::Missing semicolon"

    specify "notice with column range" $
      toByteString
        ( notice "Missing semicolon"
            & location
              ?~ ( fromFile "app.js"
                    & setSingleLinePosition (fromLine 1 & setColumnRange (ColumnRange 5 7))
                 )
        )
        `shouldBe` "::notice col=5,endColumn=7,file=app.js,line=1::Missing semicolon"

    specify "warning with column range" $
      toByteString
        ( warning "Missing semicolon"
            & location
              ?~ ( fromFile "app.js"
                    & setSingleLinePosition (fromLine 1 & setColumnRange (ColumnRange 5 7))
                 )
        )
        `shouldBe` "::warning col=5,endColumn=7,file=app.js,line=1::Missing semicolon"

    specify "warning with line range" $
      toByteString
        ( warning "Missing semicolon"
            & location ?~ (fromFile "app.js" & setLineRange (LineRange 13 16))
        )
        `shouldBe` "::warning endLine=16,file=app.js,line=13::Missing semicolon"
