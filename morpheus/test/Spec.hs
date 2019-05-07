{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Text        (Text, pack, unpack)
import           Lib              (getGQLBody, getResponseBody)
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, testCase, (@=?))
import           TestAPI          (api)

packGQLRequest :: Text -> Text
packGQLRequest x = pack $ "{\"query\":" ++ show x ++ "}"

test1 :: [Assertion]
test1 = ["ac" @=? "a" ++ "c"]

test2 :: [Assertion]
test2 = ["ab" @=? "a" ++ "b"]

helloWorld :: IO [TestTree]
helloWorld = do
  response' <- api $ packGQLRequest "{ user { name  } }"
  return [testCase "anonymus Query" ("{\"data\":{\"user\":{\"name\":\"\"}}}" @=? response')]

testFragmentLoop :: IO [TestTree]
testFragmentLoop = testByFiles "looping Fragment should throw error" "loopingFragment"

testByFiles :: Text -> Text -> IO [TestTree]
testByFiles description' fileName' = do
  query' <- getGQLBody fileName'
  response' <- getResponseBody fileName'
  result' <- api $ packGQLRequest query'
  return [testCase (unpack description') $ result' @=? response']

tests :: TestTree
tests = testGroup "Tests" [testGroup "just tests " $ testCase "-" <$> test1 ++ test2]

main :: IO ()
main = do
  iot <- helloWorld
  fragment <- testFragmentLoop
  defaultMain (testGroup "tests" $ tests : iot ++ fragment)
