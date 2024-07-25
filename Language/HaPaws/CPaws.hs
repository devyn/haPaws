{-# LANGUAGE OverloadedStrings #-}

-- | Implements parsing and serialization of cPaws ('canonical paws').
--
-- Parsing is done via a hand-written, /very simple/ lexer and parser.
module Language.HaPaws.CPaws (
  -- * Lexical analysis
  Token(..),
  SourcePosition,
  lex
) where

import           Control.Applicative

import           Data.Char
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.HaPaws.Data

import           Prelude hiding (lex)

-- | Lexical tokens.
data Token = SymbolToken Text
           | LeftParen
           | RightParen
           | LeftCurlyBrace
           | RightCurlyBrace
           | LeftQuote
           | RightQuote
           | Quote
           | Whitespace
           deriving (Show, Eq)

-- | Specifies the position of the start of a token in the original input.
type SourcePosition = (Int, Int)

-- | The output of "lex".
type Tokens = [(Token, SourcePosition)]

-- | Produces a stream of lexical tokens and their source positions from input.
lex :: Text -> Tokens
lex = flip lexExpression (1, 1)

-- | Represents the default lexing state.
lexExpression :: Text -> SourcePosition -> Tokens
lexExpression text (line, column) =
  case Text.uncons text of
       -- If we're at the end of our input, the result is an empty list.
       Nothing           -> []

       -- Single-character tokens.
       Just ('(',  rest) -> LeftParen       `advance1` lexExpression rest
       Just (')',  rest) -> RightParen      `advance1` lexExpression rest
       Just ('{',  rest) -> LeftCurlyBrace  `advance1` lexExpression rest
       Just ('}',  rest) -> RightCurlyBrace `advance1` lexExpression rest

       -- Delimited symbols: enclosed within either left-right or standard ASCII
       -- double quotes.
       Just ('"',  rest) -> Quote           `advance1` lexSymbolToken
                                                         Quote rest
       Just ('“',  rest) -> LeftQuote       `advance1` lexSymbolToken
                                                         RightQuote rest
       Just ('”',  rest) -> RightQuote      `advance1` lexExpression rest
         -- this is obviously an error, but we should let the semantic parser
         -- handle that.

       Just (ch, rest)
         | isSpace ch ->
             lexWhitespace text (line, column) -- to combine whitespace chars
         | otherwise ->
             lexIdentifier text (line, column)
               -- to combine symbol/identifier chars

  where advance1 token f = (token, (line, column)) : f (line, column + 1)
          -- Applies the supplied lexing function with the current position
          -- advanced by 1 column.

-- | Represents the lexing state for continuous whitespace. Eats whitespace
-- until non-whitespace is found, and emits a "Whitespace" token.
lexWhitespace :: Text -> SourcePosition -> Tokens
lexWhitespace text (line, column) =
  (Whitespace, (line, column)) : consumeSpaces text (line, column)
  where consumeSpaces text (line, column) =
          case Text.uncons text of
               Just ('\n', rest) -> consumeSpaces rest (line + 1, 1)
               Just (ch,   rest)
                 | isSpace ch    -> consumeSpaces rest (line, column + 1)
                 | otherwise     -> lexExpression text (line, column)
               _                 -> lexExpression text (line, column)

-- | Represents the lexing state within a symbol. Reads until the given
-- terminator is found and emits that token as well.
lexSymbolToken :: Token
                  -- ^ The terminator. Must be one of {"Quote", "RightQuote"}.
               -> Text
               -> SourcePosition
               -> Tokens
lexSymbolToken terminatorToken text origin =
  buildSymbol "" text origin
  where terminatorChar = case terminatorToken of
                              Quote      -> '"'
                              RightQuote -> '”'

        buildSymbol sym text (line, column) =
          case Text.uncons text of

            Just (ch, rest)
              -- Handle the terminator. "origin" is where this symbol started.
              | ch == terminatorChar ->
                  (SymbolToken sym, origin) :
                    (terminatorToken, (line, column)) :
                    lexExpression rest (line, column + 1)
              -- Handle newlines the same as regular characters, but add a line
              -- instead of a column.
              | ch == '\n' ->
                  buildSymbol (sym `Text.snoc` '\n') rest (line + 1, 1)
              -- Handle regular characters.
              | otherwise -> 
                  buildSymbol (sym `Text.snoc` ch) rest (line, column + 1)

            -- Terminate on everything else (end of input)
            _ ->
              (SymbolToken sym, origin) : lexExpression text (line, column)

-- | Represents the lexing state within an identifier. Reads until any special
-- char or whitespace is found.
lexIdentifier :: Text -> SourcePosition -> [(Token, SourcePosition)]
lexIdentifier text origin =
  buildIdentifier "" text origin
  where buildIdentifier i text (line, column) =
          case Text.uncons text of
               Just (ch, rest)
                 -- Terminate on whitespace or special chars. "origin" is where
                 -- this identifier started.
                 | isSpace ch ||
                   ch `elem` "(){}\"“”" ->
                     (SymbolToken i, origin) : lexExpression text (line, column)

                 -- Add anything else to the identifier.
                 | otherwise ->
                     buildIdentifier (i `Text.snoc` ch) rest (line, column + 1)

               -- Terminate on everything else (end of input)
               _ ->
                 (SymbolToken i, origin) : lexExpression text (line, column)

-- | Parses a stream of lexical tokens and positions from "lex" into a "Script",
-- or an error message.
parse :: Tokens -> IO (Either String Script)
parse tokens = either id Script <$> parseNodesUntil Nothing tokens

parseNodesUntil :: Maybe Token
                -> [(Token, SourcePosition)]
                -> IO (Either String [Node])

parseNodesUntil term (SymbolToken text : tokens) = do
  symbol <- newSymbol text
  nodes  <- parseNodesUntil term tokens
  return (ObjectNode symbol : nodes)

parseNodesUntil term (LeftParen : tokens) = do
  
