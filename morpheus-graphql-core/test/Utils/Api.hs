{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Api
  ( apiTest,
    assertion,
  )
where

import Control.Applicative (pure)
import qualified Data.Aeson as A
import Data.Aeson (decode, encode)
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Core (defaultConfig, runApi)
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    VALID,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResponseStream,
    Result (..),
    ResultT (..),
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Lib
  ( assertValidSchema,
    expectedResponse,
    getRequest,
    getResolvers,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Prelude
  ( ($),
    (==),
    IO,
    otherwise,
    show,
  )

assertion :: A.Value -> ResponseStream e Identity (Value VALID) -> IO ()
assertion expected (ResultT (Identity Success {result}))
  | Just expected == decode (encode result) = pure ()
  | otherwise =
    assertFailure $ show ("expected: \n " <> msg expected <> " \n but got: \n " <> msg result)
assertion _ (ResultT (Identity Failure {errors})) = assertFailure (show errors)

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
  actual <- runApi schema resolvers defaultConfig <$> getRequest fullPath
  expected <- expectedResponse fullPath
  assertion expected actual
