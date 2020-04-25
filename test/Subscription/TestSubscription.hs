{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.TestSubscription
  ( testSubscription
  ) where

-- import qualified Data.Text.Lazy             as LT (toStrict)
-- import           Data.Text.Lazy.Encoding    (decodeUtf8)

import           Data.Aeson                 (FromJSON, Value, decode, encode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import           Data.Morpheus.Types        (GQLRequest (..), GQLResponse (..))
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text, unpack)
import qualified Data.Text                  as T (concat)
import           GHC.Generics
import           Lib                        (getCases, getGQLBody, getResponseBody, maybeVariables)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertFailure, testCase)

-- packGQLRequest :: ByteString -> Maybe Value -> GQLRequest
-- packGQLRequest queryBS variables = GQLRequest 
--   { operationName = Nothing
--   , query = LT.toStrict $ decodeUtf8 queryBS
--   , variables
--   }

-- data Case = Case
--   { path        :: Text
--   , description :: String
--   } deriving (Generic, FromJSON)

-- testSubscription :: (GQLRequest -> IO GQLResponse) -> Text -> IO TestTree
-- testSubscription api dir = do
--   cases' <- getCases (unpack dir)
--   test' <- sequence $ testByFiles api <$> map (\x -> x {path = T.concat [dir, "/", path x]}) cases'
--   return $ testGroup (unpack dir) test'

-- testByFiles :: (GQLRequest -> IO GQLResponse) -> Case -> IO TestTree
-- testByFiles testApi Case {path, description} = do
--   testCaseQuery <- getGQLBody path
--   testCaseVariables <- maybeVariables path
--   expectedResponse <- getResponseBody path
--   actualResponse <- encode <$> testApi (packGQLRequest testCaseQuery testCaseVariables)
--   case decode actualResponse of
--     Nothing -> assertFailure "Bad Response"
--     Just response -> return $ testCase (unpack path ++ " | " ++ description) $ customTest expectedResponse response
--       where customTest expected value
--               | expected == value = return ()
--               | otherwise =
--                 assertFailure $ LB.unpack $ "expected: \n " <> encode expected <> " \n but got: \n " <> actualResponse


testSubscription :: (GQLRequest -> IO GQLResponse) -> Text -> IO TestTree
testSubscription api dir = 
  pure 
    $ testCase "test subscription"
    $ customTest ("1" :: String) ("2" :: String)
  where
    customTest expected value
      | expected == value = return ()
      | otherwise =
        assertFailure $ LB.unpack $ "expected: \n " <> encode expected <> " \n but got: \n " 
