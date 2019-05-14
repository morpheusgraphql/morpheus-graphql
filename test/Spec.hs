{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Aeson                 (FromJSON, decode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, pack)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (concat)
import           GHC.Generics
import           Lib                        (getCases, getGQLBody, getResponseBody)
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit           (assertFailure, testCase, (@=?))
import qualified TestAPI                    as Holistic (api)
import qualified UnionType.UnionType        as Union (api)

packGQLRequest :: ByteString -> ByteString
packGQLRequest x = LB.concat ["{\"query\":", LB.pack $ show x, "}"]

data Case = Case
  { path        :: Text
  , description :: String
  } deriving (Generic, FromJSON)

holisticAPITest :: IO [TestTree]
holisticAPITest = do
  cases' <- getCases "lib"
  sequence $ testByFiles Holistic.api <$> cases'

testUnionType :: IO [TestTree]
testUnionType = do
  cases' <- getCases "UnionType"
  sequence $ testByFiles Holistic.api <$> map (\x -> x {path = T.concat ["UnionType", "/", path x]}) cases'

testByFiles :: (ByteString -> IO ByteString) -> Case -> IO TestTree
testByFiles api' (Case path' description') = do
  query' <- getGQLBody path'
  response' <- getResponseBody path'
  result' <- api' $ packGQLRequest query'
  case decode result' of
    Nothing -> assertFailure "Bad Responce"
    Just x  -> return $ testCase description' $ x @=? response'

main :: IO ()
main = do
  ioTests <- holisticAPITest
  unionTest <- testUnionType
  defaultMain (testGroup "Morpheus Graphql Tests" $ ioTests ++ unionTest)
