{-# LANGUAGE RankNTypes, FlexibleContexts #-}
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

type TestInterpreter a = Interpreter (Writer [Value]) a

runTestInterpreter :: TestInterpreter a -> ForthState -> (InterpreterResult a, [Value])
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
instructionSpec =
  describe "hasInstruction" $
    let
      testWord = Word "hello"
      testProgram = def { program = [testWord] }
      testProgramEmpty = def
      (result, _) = runTestInterpreter hasInstruction testProgram
      (resultE, _) = runTestInterpreter hasInstruction testProgramEmpty
      (resultN, _) = runTestInterpreter nextInstruction testProgram
      (resultNextPopped, _) = runTestInterpreter popInstructionStack testProgram
    in do
      it "correctly checks that a program is non-empty" $
        result `shouldBe` Right (True, testProgram)
      it "correctly checks that a program is empty" $
        resultE `shouldBe` Right (False, testProgramEmpty)
      it "correctly lifts the next test program off the stack" $
        resultN `shouldBe` Right (testWord, testProgram)
      it "correctly pops the instruction stack and returns the popped instruction" $
        resultNextPopped `shouldBe` Right (testWord, def)


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
