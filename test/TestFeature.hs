{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TestFeature
  ( testFeature,
  )
where

import Data.Aeson (Value, decode, encode)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import Data.Morpheus.Types (GQLRequest (..), GQLResponse (..))
import Data.Semigroup ((<>))
import Data.Text (unpack)
import qualified Data.Text.Lazy as LT (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Lib (getGQLBody, getResponseBody, maybeVariables)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Types
  ( Case (..),
    Name,
    testWith,
  )

packGQLRequest :: ByteString -> Maybe Value -> GQLRequest
packGQLRequest queryBS variables =
  GQLRequest
    { operationName = Nothing,
      query = LT.toStrict $ decodeUtf8 queryBS,
      variables
    }

testFeature :: (GQLRequest -> IO GQLResponse) -> Name -> IO TestTree
testFeature api = testWith (testByFiles api)

testByFiles :: (GQLRequest -> IO GQLResponse) -> Case -> IO TestTree
testByFiles testApi Case {path, description} = do
  testCaseQuery <- getGQLBody path
  testCaseVariables <- maybeVariables path
  expectedResponse <- getResponseBody path
  actualResponse <- encode <$> testApi (packGQLRequest testCaseQuery testCaseVariables)
  case decode actualResponse of
    Nothing -> assertFailure "Bad Response"
    Just response -> return $ testCase (unpack path ++ " | " ++ description) $ customTest expectedResponse response
      where
        customTest expected value
          | expected == value = return ()
          | otherwise =
            assertFailure $ LB.unpack $ "expected: \n " <> encode expected <> " \n but got: \n " <> actualResponse
