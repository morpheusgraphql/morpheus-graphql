module Main
  ( main
  ) where

import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@=?))

test1 :: [Assertion]
test1 = ["ac" @=? "a" ++ "c"]

test2 :: [Assertion]
test2 = ["ab" @=? "a" ++ "b"]

ioTests :: IO [TestTree]
ioTests = return [testCase "" ("ab" @=? "a" ++ "b")]

tests :: TestTree
tests = testGroup "unit" [testGroup "Nullary constructors" $ fmap (testCase "-") (test1 ++ test2)]

main :: IO ()
main = do
  iot <- ioTests
  let allTests = tests : iot
  defaultMain (testGroup "tests" allTests)
