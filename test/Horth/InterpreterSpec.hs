{-# LANGUAGE OverloadedLists, RankNTypes, FlexibleContexts #-}
module Horth.InterpreterSpec where

import Test.Hspec
import Horth
import Data.Default (def)
import Control.Monad.Writer (Writer, runWriter)

spec :: Spec
spec = describe "Interpreter" $ do
  popSpec
  emitSpec
  instructionSpec
  lookupSpec

type TestInterpreter a = Interpreter (Writer [Value]) a

runTestInterpreter :: TestInterpreter a ->
                      ForthState ->
                      (InterpreterResult a, [Value])
runTestInterpreter i s = runWriter $ evaluateInterpreter i s

emitSpec :: Spec
emitSpec =
  describe "emit" $
      let
        emitOne =
          emit $ Number 1
        (_, emitted) = runTestInterpreter emitOne def
      in it "emit is an interpreter and emits something" $
        emitted `shouldBe` [Number 1]

instructionSpec :: Spec
instructionSpec = do
  describe "hasInstruction" $
    let
      testWord = Word "hello"
      testProgram = def { program = [testWord] }
      testProgramEmpty = def
      (result, _) = runTestInterpreter hasInstruction testProgram
      (resultE, _) = runTestInterpreter hasInstruction testProgramEmpty
      (resultN, _) = runTestInterpreter nextInstruction testProgram
      (resultNextPopped, _) = runTestInterpreter popInstruction testProgram
    in do
      it "checks that a program is non-empty" $
        result `shouldBe` Right (True, testProgram)
      it "checks that a program is empty" $
        resultE `shouldBe` Right (False, testProgramEmpty)
      it "lifts the next test program off the stack" $
        resultN `shouldBe` Right (testWord, testProgram)
      it "pops the instruction stack and returns the popped instruction" $
        resultNextPopped `shouldBe` Right (testWord, def)
  describe "pushInstruction" $
    let
      testWord = Word "hello"
      nonEmptyProgram = def { program = [testWord] }
      (result, _) = runTestInterpreter (pushInstruction testWord) def
    in it "pushes a word onto the stack" $
      result `shouldBe` Right ((), nonEmptyProgram)

popSpec :: Spec
popSpec =
  let
    testState = def { stack = [Number 1] }
  in
    describe "pop" $
      let
        (result, emitted) = runTestInterpreter pop testState
      in do
        it "pops the first element off the stack" $
          result `shouldBe` Right (Number 1, def)
        it "writes nothing" $
          emitted `shouldBe` []
        it "returns a stack underflow when there is nothing on the stack" $
          let
            (result, _) = runTestInterpreter pop def
          in result `shouldBe` Left StackUnderflow

lookupSpec :: Spec
lookupSpec = describe "lookupWord" $
  let
    word1 = "word"
    value1 = Number 1
    testState = def { dictionary = [(word1, Variable value1)]}
  in do
    it "looks up a variable and does not change the stack" $
      let
        (result, _) = runTestInterpreter (lookupWord word1) testState
      in result `shouldBe` Right (Variable value1, testState)
    it "throws a word not found error when the variable is not found" $
      let
        nonExistantWord = "notexistant"
        (notFoundResult, _) = runTestInterpreter (lookupWord nonExistantWord) testState
      in notFoundResult `shouldBe` Left (WordNotFound nonExistantWord)
