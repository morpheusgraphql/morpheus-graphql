{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Case.ApolloRequest
  ( testApolloRequest,
    SubM,
  )
where

-- import qualified Data.Text.Lazy             as LT (toStrict)
-- import           Data.Text.Lazy.Encoding    (decodeUtf8)
import Data.ByteString.Lazy.Char8 (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as LB
--   ( unpack,
--   )
import Data.Morpheus.Types
  ( Input,
    Stream,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( GQLChannel (..),
    WS,
    connect,
    empty,
  )
import Data.Semigroup ((<>))
import Subscription.Utils
  ( SubM,
    TrailState (..),
    expectedResponse,
    trail,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( testCase,
  )
import Types
  ( Case (..),
    Name,
    testWith,
  )

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

testSubCase ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testSubCase api = do
  input <- connect
  TrailState {inputs, outputs, store} <- trail api input (TrailState ["{ \"type\":\"bla\" }"] [] empty)
  pure
    $ testCase "fail on unknown request type"
    $ expectedResponse
      ["Unknown Request type \"bla\"."]
      outputs

testApolloRequest ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testApolloRequest api = do
  subscription <- testSubCase api
  return $ testGroup "ApolloRequest" [subscription]
