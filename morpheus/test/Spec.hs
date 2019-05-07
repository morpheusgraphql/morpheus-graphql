{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Text        (Text, pack, unpack)
import           Lib              (getGQLBody, getResponseBody)
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@=?))
import           TestAPI          (api)

packGQLRequest :: Text -> Text
packGQLRequest x = pack $ "{\"query\":" ++ show x ++ "}"

unknownArguments :: IO TestTree
unknownArguments = testByFiles "unknown Argument should Throw an Error" "unknownArguments"

testFragmentLoop :: IO TestTree
testFragmentLoop = testByFiles "looping Fragment should throw an Error" "loopingFragment"

testByFiles :: Text -> Text -> IO TestTree
testByFiles description' fileName' = do
  query' <- getGQLBody fileName'
  response' <- getResponseBody fileName'
  result' <- api $ packGQLRequest query'
  return $ testCase (unpack description') $ result' @=? response'

main :: IO ()
main = do
  ioTests <- sequence [unknownArguments, testFragmentLoop]
  defaultMain (testGroup "Integration Tests" ioTests)
