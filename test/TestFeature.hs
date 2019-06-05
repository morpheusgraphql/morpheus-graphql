{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TestFeature
  ( testFeature
  ) where

import           Data.Aeson                 (FromJSON, decode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (concat, pack)
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as T (concat)
import           GHC.Generics
import           Lib                        (getCases, getGQLBody, getResponseBody, maybeVariables)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertFailure, testCase, (@=?))

packGQLRequest :: ByteString -> ByteString -> ByteString
packGQLRequest x variables' = LB.concat ["{\"query\":", LB.pack $ show x, ",\"variables\":", variables', "}"]

data Case = Case
  { path        :: Text
  , description :: String
  } deriving (Generic, FromJSON)

testFeature :: (ByteString -> IO ByteString) -> Text -> IO TestTree
testFeature api' dir' = do
  cases' <- getCases (unpack dir')
  test' <- sequence $ testByFiles api' <$> map (\x -> x {path = T.concat [dir', "/", path x]}) cases'
  return $ testGroup (unpack dir') test'

testByFiles :: (ByteString -> IO ByteString) -> Case -> IO TestTree
testByFiles api' (Case path' description') = do
  query' <- getGQLBody path'
  response' <- getResponseBody path'
  variables' <- maybeVariables path'
  result' <- api' $ packGQLRequest query' variables'
  case decode result' of
    Nothing  -> assertFailure "Bad Response"
    Just res -> return $ testCase (unpack path' ++ " | " ++ description') $ response' @=? res
