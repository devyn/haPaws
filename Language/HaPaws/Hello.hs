module Language.HaPaws.Hello (
  hello
) where

hello :: String -> String

hello = ("Hello, " ++) . (++ "!")
