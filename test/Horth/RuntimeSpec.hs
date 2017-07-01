{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Horth.RuntimeSpec where

import Test.Hspec
import Horth
import Data.Default (def)
import Control.Monad.Writer (Writer, runWriter)

spec :: Spec
spec = describe "Interpreter" $ do
  popSpec
  emitSpec

type TestInterpreter a = Interpreter (Writer [Value]) a

runTestInterpreter :: TestInterpreter a -> Program -> ForthState -> (InterpreterResult a, [Value])
runTestInterpreter i p s = runWriter $ evaluateInterpreter i p s

emitSpec :: Spec
emitSpec =
  describe "emit" $
      let
        emitOne =
          emit $ Number 1
        (_, emitted) = runTestInterpreter emitOne [] def
      in it "emit is an interpreter and emits something" $
        emitted `shouldBe` [Number 1]

popSpec :: Spec
popSpec =
  let
    testState = def { stack = [Number 1] }
  in
    describe "pop" $
      let
        (result, emitted) = runTestInterpreter pop [] testState
      in do
        it "pops the first element off the stack" $
          result `shouldBe` Right (Number 1, def)
        it "writes nothing" $
          emitted `shouldBe` []
        it "returns a stack underflow when there is nothing on the stack" $
          let
            (result, _) = runTestInterpreter pop [] def
          in result `shouldBe` Left StackUnderflow
