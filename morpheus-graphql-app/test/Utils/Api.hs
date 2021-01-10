{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Api
  ( apiTest,
    assertion,
  )
where

import qualified Data.Aeson as A
import Data.Aeson (decode, encode)
import Data.Morpheus.App.Internal.Resolving
  ( ResponseStream,
    ResultT (..),
  )
import Data.Morpheus.Core (mkApp, runAppStream)
import Data.Morpheus.Types.IO
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    VALID,
    Value (..),
  )
import Data.Text (unpack)
import Relude
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( testCase,
  )
import Utils.Utils
  ( assertValidSchema,
    caseFailure,
    expectedResponse,
    getRequest,
    getResolvers,
  )

assertion :: A.Value -> ResponseStream e Identity (Value VALID) -> IO ()
assertion expected (ResultT (Identity actual))
  | Just expected == decode actualValue = pure ()
  | otherwise = caseFailure (encode expected) actualValue
  where
    actualValue = encode (renderResponse actual)

apiTest :: FieldName -> [FieldName] -> TestTree
apiTest apiPath requestPath =
  testGroup (unpack $ readName apiPath) $
    fmap (testApiRequest apiPath) requestPath

testApiRequest ::
  FieldName ->
  FieldName ->
  TestTree
testApiRequest apiPath path = testCase (unpack $ readName path) $ do
  schema <- assertValidSchema apiPath
  resolvers <- getResolvers apiPath
  let fullPath = apiPath <> "/" <> path
  let api = mkApp schema resolvers
  actual <- runAppStream api <$> getRequest fullPath
  expected <- expectedResponse fullPath
  assertion expected actual
