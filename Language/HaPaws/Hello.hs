-- | A useless module while I test out all of the boilerplate (specs,
-- documentation, etc.) and get that ready.
module Language.HaPaws.Hello (
  hello
) where

-- | Returns a string greeting the argument, probably someone or something's
-- name, with 'Hello'.
hello :: String -> String
hello = ("Hello, " ++) . (++ "!")
