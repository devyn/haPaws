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
      map fst (lex "Hello") `shouldBe` [SymbolToken "Hello"]

    it "analyzes multiple labels" $ do
      let tokens = map fst (lex "Hello world")
      head tokens `shouldBe` SymbolToken "Hello"
      last tokens `shouldBe` SymbolToken "world"

    it "preserves whitespace and groups it" $ do
      let tokens = map fst (lex "Hello world")
      filter (== Whitespace) tokens `shouldBe` [Whitespace]

    it "preserves leading and trailing whitespace" $ do
      let tokens = map fst (lex "  abc  ")
      head tokens `shouldBe` Whitespace
      last tokens `shouldBe` Whitespace

    it "analyzes labels in plain ASCII double quotes" $ do
      map fst (lex "\"Hello world\"") `shouldBe`
        [Quote, SymbolToken "Hello world", Quote]

    it "analyzes labels in fancy left and right double quotes" $ do
      map fst (lex "“Hello world”") `shouldBe`
        [LeftQuote, SymbolToken "Hello world", RightQuote]

    it "analyzes delimited subexpressions" $ do
      map fst (lex "(Hello world)") `shouldBe`
        [LeftParen, SymbolToken "Hello", Whitespace, SymbolToken "world"
        ,RightParen]

    it "analyzes execution literals" $ do
      map fst (lex "{Hello world}") `shouldBe`
        [LeftCurlyBrace, SymbolToken "Hello", Whitespace, SymbolToken "world"
        ,RightCurlyBrace]

    it "analyzes nested subexpressions and execution literals" $ do
      map fst (lex "{Hello ({world})}") `shouldBe`
        [LeftCurlyBrace, SymbolToken "Hello", Whitespace, LeftParen
        ,LeftCurlyBrace, SymbolToken "world", RightCurlyBrace, RightParen
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
                        ,SymbolToken "Hello"
                        ,Whitespace
                        ,SymbolToken "my"
                        ,Whitespace
                        ,Quote
                        ,SymbolToken "dog, Tom"
                        ,Quote
                        ,Whitespace
                        ,LeftCurlyBrace
                        ,SymbolToken "is"
                        ,Whitespace
                        ,LeftCurlyBrace
                        ,SymbolToken "actually"
                        ,RightCurlyBrace
                        ,RightCurlyBrace
                        ,Whitespace
                        ,LeftParen
                        ,SymbolToken "a"
                        ,Whitespace
                        ,LeftQuote
                        ,SymbolToken "prince"
                        ,RightQuote
                        ,RightParen
                        ,Whitespace]
