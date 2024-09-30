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
    context "with no properties" do
      specify "" $
        toByteString (debug "Set the Octocat variable")
          `shouldBe` "::debug::Set the Octocat variable"

      specify "" $
        toByteString (error "Missing semicolon")
          `shouldBe` "::error::Missing semicolon"

    context "with file" do
      specify "" $
        toByteString (error "Missing semicolon" & location ?~ "app.js")
          `shouldBe` "::error file=app.js::Missing semicolon"

    context "with line number" do
      specify "" $
        toByteString
          ( error "Missing semicolon"
              & location ?~ (inFile "app.js" & position ?~ atLine 1)
          )
          `shouldBe` "::error file=app.js,line=1::Missing semicolon"

    context "with column" do
      specify "" $
        toByteString
          ( error "Missing semicolon"
              & location
                ?~ (inFile "app.js" & position ?~ (atLine 7 & extent ?~ WithinLine (atColumn 64)))
          )
          `shouldBe` "::error col=64,file=app.js,line=7::Missing semicolon"

    context "with column range" do
      specify "" $
        toByteString
          ( notice "Missing semicolon"
              & location
                ?~ ( inFile "app.js"
                      & position ?~ (atLine 1 & extent ?~ WithinLine (atColumn 5 & endColumn ?~ 7))
                   )
          )
          `shouldBe` "::notice col=5,endColumn=7,file=app.js,line=1::Missing semicolon"

      specify "" $
        toByteString
          ( warning
              "Missing semicolon"
              & location
                ?~ ( inFile "app.js"
                      & position ?~ (atLine 1 & extent ?~ WithinLine (atColumn 15 & endColumn ?~ 32))
                   )
          )
          `shouldBe` "::warning col=15,endColumn=32,file=app.js,line=1::Missing semicolon"

    context "with line range" do
      it "" $
        toByteString
          ( warning "Missing semicolon"
              & location ?~ (inFile "app.js" & position ?~ (atLine 13 & extent ?~ ToLine 16))
          )
          `shouldBe` "::warning endLine=16,file=app.js,line=13::Missing semicolon"
