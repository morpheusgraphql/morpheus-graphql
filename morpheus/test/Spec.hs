{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, pack)
import           Data.Text                  (Text)
import           Lib                        (getGQLBody, getInfo, getResponseBody)
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit           (assertFailure, testCase, (@=?))
import           TestAPI                    (api)

packGQLRequest :: ByteString -> ByteString
packGQLRequest x = LB.concat ["{\"query\":", LB.pack $ show x, "}"]

holisticAPITest :: IO [TestTree]
holisticAPITest = sequence $ testByFiles <$> ["loopingFragment", "unknownArguments", "unusedVariables"]

testByFiles :: Text -> IO TestTree
testByFiles fileName' = do
  description' <- getInfo fileName'
  query' <- getGQLBody fileName'
  response' <- getResponseBody fileName'
  result' <- api $ packGQLRequest query'
  case decode result' of
    Nothing -> assertFailure "Bad Responce"
    Just x  -> return $ testCase description' $ x @=? response'

main :: IO ()
main = do
  ioTests <- holisticAPITest
  defaultMain (testGroup "Morpheus Graphql Tests" ioTests)
