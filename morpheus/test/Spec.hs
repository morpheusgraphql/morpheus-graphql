{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Text
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@=?))
import           TestAPI          (api)

packGQLRequest :: Text -> Text
packGQLRequest _ = "{\"query\":\"{ user { name  } }\"}"

test1 :: [Assertion]
test1 = ["ac" @=? "a" ++ "c"]

test2 :: [Assertion]
test2 = ["ab" @=? "a" ++ "b"]

ioTests :: IO [TestTree]
ioTests = do
  response' <- api $ packGQLRequest "hello world Test"
  return [testCase "" ("{\"data\":{\"user\":{\"name\":\"\"}}}" @=? response')]

tests :: TestTree
tests = testGroup "Tests" [testGroup "just tests " $ testCase "-" <$> test1 ++ test2]

main :: IO ()
main = do
  iot <- ioTests
  defaultMain (testGroup "tests" $ tests : iot)
