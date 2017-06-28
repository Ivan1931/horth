module Horth (
  repl
) where

import Data.Map (fromList, Map)

data Token =
  Word String | -- Just a sequence of characters
  String String | -- Characters delimited by ""
  Number Int | -- Sequence of numbers
  Colon String -- : sentence started with a sequence of colons and ended with ;
  deriving (Eq, Show)

repl :: IO ()
repl = undefined
