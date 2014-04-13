module Spec.Language.HaPaws.Hello (spec) where

import           Test.Hspec

import           Language.HaPaws.Hello

spec = describe "Language.HaPaws.Hello" $ do

  describe "hello" $ do
    it "returns a greeting containing \"Hello\"" $ do
      hello "Devyn" `shouldContain` "Hello"

    it "returns a greeting directed at its argument" $ do
      hello "Devyn" `shouldContain` "Devyn"
