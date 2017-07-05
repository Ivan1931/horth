{-# LANGUAGE OverloadedLists, RankNTypes, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Horth  where

import           Control.Exception    (Exception, catch)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.State  (StateT, get, put, runStateT)
import           Control.Monad.Writer (MonadWriter, tell)
import           Control.Monad (when)
import           Data.Default         (Default, def)
import           Data.Map             (Map, fromList, lookup, insert)
import           Prelude hiding (lookup)

data Value =
  Number Int | -- represents a number
  String String -- Characters delimited by ""
  deriving (Eq, Show, Read)

data Instruction =
  Value Value | -- Basic value
  Word String | -- Just a sequence of characters that represents forth word
  Definition Program | -- : sentence started with a sequence of colons and ended with ;
  Loop Program | -- Represents a loop structure of the forth program
  If Program Program -- Represents the branches of an if - else conditional
  deriving (Eq, Show)

type Program = [Instruction]  -- for now represent programs as list of Instruction words
type Stack = [Value] -- for now represent the stack as a list

data DictionaryEntry =
  Constant Value | -- Wrapper for storing a constant a variable that can't be modified
  Variable Value | -- Wrapper for storing a variable which can be changed
  WordEntry Program -- Wrapper for a reference to a block that is both mutable
  deriving (Eq, Show)

data ForthState = ForthState {
  dictionary :: Map String DictionaryEntry,
  stack      :: Stack,
  program    :: Program
} deriving (Eq, Show)

instance Default ForthState where
  def = ForthState {
    dictionary = [],
    stack      = [],
    program    = []
  }

data ForthError = ParseError
                | WordNotFound String -- Happens when attempting to lookup a word that does not exist
                | StackUnderflow  -- Happens when pop is called on empty stack
                | EmptyInstructionStack -- Happens when attempting to get next instruction on null program
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

nextInstruction :: Interpreter m Instruction
nextInstruction = do
  state <- get
  case program state of
    [] -> throwError EmptyInstructionStack
    (x:xs) -> return x

popInstruction :: Interpreter m Instruction
popInstruction = do
  state <- get
  case program state of
    [] -> throwError EmptyInstructionStack
    (x: xs) -> do
      put state { program = [] }
      return x

push :: Value -> Interpreter m ()
push value = undefined

pushInstruction :: Instruction -> Interpreter m ()
pushInstruction i = do
  state <- get
  put $ state { program = i : program state }

lookupWord :: String -> Interpreter m DictionaryEntry
lookupWord s = do
  values <- dictionary <$> get
  case lookup s values of
    Just instructions -> return instructions
    Nothing           -> throwError $ WordNotFound s

type InterpreterResult a = Either ForthError (a, ForthState)

evaluateInterpreter :: FIO m => Interpreter m a -> ForthState -> m (InterpreterResult a)
evaluateInterpreter i state = runExceptT (runStateT i state)

interpretWord :: String -> Interpreter m ()
interpretWord word = do
  word <- lookupWord word
  case word of
    Constant  constant -> push constant
    Variable  variable -> push variable
    WordEntry program  -> evaluateProgram program

evaluateInstruction :: Instruction -> Interpreter m ()
evaluateInstruction = undefined

evaluateProgram :: Program -> Interpreter m ()
evaluateProgram = undefined

interpreter :: Interpreter m ()
interpreter = do
  continue <- hasInstruction
  when continue $ do
    i <- popInstruction
    case i of
      Value value           -> push value
      Word word             -> interpretWord word
      Definition definition -> evaluateProgram definition
    interpreter

evaluateForth :: FIO m => ForthState -> m (InterpreterResult ())
evaluateForth = evaluateInterpreter interpreter

repl :: IO ()
repl = undefined
