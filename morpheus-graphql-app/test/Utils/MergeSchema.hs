{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.MergeSchema
  ( runMergeTest,
  )
where

import qualified Data.ByteString.Lazy as L (readFile)
import Data.Morpheus.App
  ( App (..),
    AppData (..),
    mkApp,
    mkApp,
  )
import Data.Morpheus.App.Internal.Resolving
  ( resultOr,
  )
import Data.Morpheus.Core
  ( parseSchema,
    render,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Relude
import Test.Morpheus.Utils
  ( FileUrl (..),
    assertEqualFailure,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Utils.Utils
  ( getResolver,
    testRequest,
  )

readSchema :: FilePath -> IO (Schema VALID)
readSchema p = L.readFile p >>= (resultOr (fail . show) pure . parseSchema)

loadApi :: FilePath -> IO (App () Identity)
loadApi url = do
  schema <- readSchema (url <> ".gql")
  resolvers <- getResolver (url <> ".json")
  pure $ mkApp schema resolvers

readApi :: FileUrl -> IO (App () Identity)
readApi url = do
  schema <- loadApi (toString url <> "/app")
  extension <- loadApi (toString url <> "/ext")
  pure (schema <> extension)

runMergeTest :: FileUrl -> [FileUrl] -> [TestTree]
runMergeTest url assets = testSchema url : map (testRequest (readApi url)) assets

testSchema :: FileUrl -> TestTree
testSchema caseUrl = testCase
  "schema"
  $ do
    api <- readApi caseUrl
    schema <- readSchema (toString caseUrl <> "/rendering.gql")
    schemaAssertion api schema

schemaAssertion :: App () Identity -> Schema VALID -> IO ()
schemaAssertion (App AppData {appSchema}) expectedSchema = assertEqualFailure (render expectedSchema) (render appSchema)
schemaAssertion (FailApp gqlerror) _ = assertFailure $ " error: " <> show gqlerror
