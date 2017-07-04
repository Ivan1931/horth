{-# LANGUAGE OverloadedLists, RankNTypes, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Horth  where

import           Control.Exception    (Exception, catch)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.State  (StateT, get, put, runStateT)
import           Control.Monad.Writer (MonadWriter, tell)
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

data ForthError = ParseError
                | StackUnderflow  -- Try to pop something off an empty stack
                | Stackoverflow  -- We have exceeded the stack size (impossible since this is haskell)
                | EmptyInstructionStack -- We don't have any instructions left to do
                deriving (Show, Eq)

instance Exception ForthError

type Interpreter m a = (FIO m) => StateT ForthState (ExceptT ForthError m) a

class (Monad m) => FIO m where
  emit :: Value -> m ()
  consume :: m Value

instance FIO IO where
  emit = print . show
  consume = do
    line <- getLine
    return (read line :: Value)

instance (Monad m, MonadWriter [Value] m) => FIO m where
  emit x = tell [x]
  consume = undefined

pop :: Interpreter m Value
pop = do
  state <- get
  case stack state of
    [] -> throwError StackUnderflow
    (top:rest) -> do
      put $ state { stack = rest }
      return top

pop2 :: Interpreter m (Value, Value)
pop2 = do
  a <- pop
  b <- pop
  return (a, b)

hasInstruction :: Interpreter m Bool
hasInstruction = do
  state <- get
  return $ case program state of
    [] -> False
    _  -> True

nextInstruction :: Interpreter m Forth
nextInstruction = do
  state <- get
  case program state of
    [] -> throwError EmptyInstructionStack
    (x:xs) -> return x

popInstructionStack :: Interpreter m Forth
popInstructionStack = do
  state <- get
  case program state of
    [] -> throwError EmptyInstructionStack
    (x: xs) -> do
      put state { program = [] }
      return x

type InterpreterResult a = Either ForthError (a, ForthState)

evaluateInterpreter :: FIO m => Interpreter m a -> ForthState -> m (InterpreterResult a)
evaluateInterpreter i state = runExceptT (runStateT i state)

interpreter :: Interpreter m Value
interpreter = pop

evaluateForth :: FIO m => ForthState -> m (InterpreterResult Value)
evaluateForth = evaluateInterpreter interpreter

repl :: IO ()
repl = undefined
