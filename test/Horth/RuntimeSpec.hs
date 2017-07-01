{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Horth.RuntimeSpec where

import Test.Hspec
import Horth
import Data.Default (def)
import Control.Monad.Writer (Writer, runWriter)

spec :: Spec
spec = describe "Runtime evaluation spec" $ do
  popSpec
  truthSpec

type TestInterpreter = Interpreter (Writer [Value]) Value

runTestInterpreter :: TestInterpreter -> Program -> ForthState -> (InterpreterResult Value, [Value])
runTestInterpreter i p s = runWriter $ evaluateInterpreter i p s

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

truthSpec :: Spec
truthSpec = describe "The truth" $ it "true is true" $ True `shouldBe` True
