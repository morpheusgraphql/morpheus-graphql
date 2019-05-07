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

requestTests :: [(Text, Text)]
requestTests =
  [ ("loopingFragment", "looping Fragment should throw an Error")
  , ("unknownArguments", "unknown Argument should Throw an Error")
  , ("unusedVariables", "unused variable schould throw Error")
  ]

testByFiles :: (Text, Text) -> IO TestTree
testByFiles (fileName', description') = do
  query' <- getGQLBody fileName'
  response' <- getResponseBody fileName'
  result' <- api $ packGQLRequest query'
  return $ testCase (unpack description') $ result' @=? response'

main :: IO ()
main = do
  ioTests <- sequence $ testByFiles <$> requestTests
  defaultMain (testGroup "Integration Tests" ioTests)
