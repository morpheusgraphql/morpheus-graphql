{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, pack)
import           Data.Text                  (Text, unpack)
import           Lib                        (getGQLBody, getResponseBody)
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit           (assertFailure, testCase, (@=?))
import           TestAPI                    (api)

packGQLRequest :: ByteString -> ByteString
packGQLRequest x = LB.concat ["{\"query\":", LB.pack $ show x, "}"]

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
  case decode result' of
    Nothing -> assertFailure "Bad Responce"
    Just x  -> return $ testCase (unpack description') $ x @=? response'

main :: IO ()
main = do
  ioTests <- sequence $ testByFiles <$> requestTests
  defaultMain (testGroup "Integration Tests" ioTests)
