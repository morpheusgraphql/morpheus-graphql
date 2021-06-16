{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Rendering
  ( runRenderingTest,
  )
where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Morpheus.Core
  ( defaultConfig,
    parseRequestWith,
    parseSchema,
    render,
  )
import Data.Morpheus.Internal.Ext
  ( Eventless,
    Result (..),
    resultOr,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Operation,
    Schema,
    VALID,
  )
import Relude hiding (ByteString)
import Test.Morpheus.Utils
  ( FileUrl (..),
    assertEqualFailure,
    assertValidSchemaFailure,
    getVariables,
    readQueryFile,
    readSchemaFile,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )

assertion :: ByteString -> Eventless (Operation VALID) -> IO ()
assertion expected Success {result} = assertEqualFailure expected (render result)
assertion _ Failure {errors} = assertFailure $ LB.unpack (encode errors)

runRenderingTest :: FileUrl -> [FileUrl] -> [TestTree]
runRenderingTest url = map (testRendering url)

assertValidSchema :: String -> IO (Schema VALID)
assertValidSchema = readSchemaFile >=> resultOr assertValidSchemaFailure pure . parseSchema

getRequest :: String -> IO GQLRequest
getRequest p =
  GQLRequest
    Nothing
    <$> readQueryFile p
    <*> getVariables p

testRendering ::
  FileUrl ->
  FileUrl ->
  TestTree
testRendering schemaPath path = testCase (fileName path) $ do
  schema <- assertValidSchema (toString schemaPath)
  let fullPath = toString path
  actual <- parseRequestWith defaultConfig schema <$> getRequest fullPath
  expected <- L.readFile (toString $ fullPath <> "/rendering.gql")
  assertion expected actual
