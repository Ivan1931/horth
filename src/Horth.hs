{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes      #-}

module Horth (
  repl,
  evaluateForth,
  Value(..),
  Forth(..)
) where

import           Control.Exception    (Exception, catch, throw)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.State  (StateT, get, put, runStateT)
import           Data.Default         (Default, def)
import           Data.Map             (Map, fromList)

data Value =
  Number Int | -- represents a number
  String String -- Characters delimited by ""
  deriving (Eq, Show, Read)

data Forth =
  Value Value | -- Basic value
  Word String | -- Just a sequence of characters that represents forth word
  Block Program -- : sentence started with a sequence of colons and ended with ;
  deriving (Eq, Show)

type Program = [Forth]  -- for now represent programs as list of forth words
type Stack = [Value] -- for now represent the stack as a list
type StackOp = Stack -> Stack -- for now represent stack as function on top of stack
type Constants = Map String Value
type Variables = Map String Value

class (Monad m) => FIO m where
  emit :: Value -> m ()
  consume :: m Value

data ForthState = ForthState {
  constants :: Constants,
  variables :: Variables,
  stack     :: Stack,
  program   :: Program
} deriving (Eq, Show)

instance Default ForthState where
  def = ForthState {
    constants = [],
    variables = [],
    stack = [],
    program = []
  }

data ForthError = ParseError | StackUnderflow | Stackoverflow
                deriving (Show, Eq)

instance Exception ForthError

type Runtime m a = (FIO m) => StateT ForthState (ExceptT ForthError m) a

instance FIO IO where
  emit = print . show
  consume = do
    line <- getLine
    return (read line :: Value)

testState =
  ForthState {
    constants = [],
    variables = [],
    stack = [Number 1],
    program = []
  }

pop :: Runtime m Value
pop = do
  state <- get
  case stack state of
    [] -> throw StackUnderflow
    (top:rest) -> do
      put $ state { stack = rest }
      return top

interpreter :: Runtime m Value
interpreter = pop

evaluateForth :: FIO m => Program -> m (Either ForthError (Value, ForthState))
evaluateForth program = runExceptT (runStateT interpreter def { stack = [Number 1] })

repl :: IO ()
repl = undefined
