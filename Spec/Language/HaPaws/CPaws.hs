{-# LANGUAGE OverloadedStrings #-}

module Spec.Language.HaPaws.CPaws (spec) where

import           Test.Hspec
import qualified Data.Text as Text
import           Data.Text (Text)
import           Prelude hiding (lex)

import           Language.HaPaws.CPaws

spec = describe "Language.HaPaws.CPaws" $ do
  describe "lex" $ do
    it "analyzes an empty string" $ do
      lex "" `shouldBe` []

    it "analyzes a string with nothing it it but whitespace" $ do
      map fst (lex "     ") `shouldContain` [Whitespace]

    it "analyzes a single identifier" $ do
      map fst (lex "Hello") `shouldBe` [Symbol "Hello"]

    it "analyzes multiple identifiers" $ do
      let tokens = map fst (lex "Hello world")
      head tokens `shouldBe` Symbol "Hello"
      last tokens `shouldBe` Symbol "world"

    it "preserves whitespace and groups it" $ do
      let tokens = map fst (lex "Hello world")
      filter (== Whitespace) tokens `shouldBe` [Whitespace]

    it "preserves leading and trailing whitespace" $ do
      let tokens = map fst (lex "  abc  ")
      head tokens `shouldBe` Whitespace
      last tokens `shouldBe` Whitespace

    it "analyzes symbols in plain ASCII double quotes" $ do
      map fst (lex "\"Hello world\"") `shouldBe`
        [Quote, Symbol "Hello world", Quote]

    it "analyzes symbols in fancy left and right double quotes" $ do
      map fst (lex "“Hello world”") `shouldBe`
        [LeftQuote, Symbol "Hello world", RightQuote]

    it "analyzes delimited subexpressions" $ do
      map fst (lex "(Hello world)") `shouldBe`
        [LeftParen, Symbol "Hello", Whitespace, Symbol "world", RightParen]

    it "analyzes execution literals" $ do
      map fst (lex "{Hello world}") `shouldBe`
        [LeftCurlyBrace, Symbol "Hello", Whitespace, Symbol "world"
        ,RightCurlyBrace]

    it "analyzes nested subexpressions and execution literals" $ do
      map fst (lex "{Hello ({world})}") `shouldBe`
        [LeftCurlyBrace, Symbol "Hello", Whitespace, LeftParen
        ,LeftCurlyBrace, Symbol "world", RightCurlyBrace, RightParen
        ,RightCurlyBrace]

    it "records line numbers for each token" $ do
      map (fst . snd) (lex "Hello\nworld") `shouldBe`
        [1, 1, 2]

    it "records column numbers for each token" $ do
      map (snd . snd) (lex "Hello world") `shouldBe`
        [1, 6, 7]

    it "resets the column number after a newline" $ do
      map snd (lex "Hello world\nhi") `shouldBe`
        [(1, 1)
        ,(1, 6)
        ,(1, 7)
        ,(1, 12)
        ,(2, 1)]

    -- Simple and ugly catch-all. If this ever fails and all of the above pass
    -- then we need to make more tests.
    it "analyzes a tricky example containing every kind of token" $ do
      let tokens = map fst . lex $ "   Hello\nmy \"dog, Tom\" " `Text.append`
                                   "{is {actually}} \n(a “prince”) "
      tokens `shouldBe` [Whitespace
                        ,Symbol "Hello"
                        ,Whitespace
                        ,Symbol "my"
                        ,Whitespace
                        ,Quote
                        ,Symbol "dog, Tom"
                        ,Quote
                        ,Whitespace
                        ,LeftCurlyBrace
                        ,Symbol "is"
                        ,Whitespace
                        ,LeftCurlyBrace
                        ,Symbol "actually"
                        ,RightCurlyBrace
                        ,RightCurlyBrace
                        ,Whitespace
                        ,LeftParen
                        ,Symbol "a"
                        ,Whitespace
                        ,LeftQuote
                        ,Symbol "prince"
                        ,RightQuote
                        ,RightParen
                        ,Whitespace]
