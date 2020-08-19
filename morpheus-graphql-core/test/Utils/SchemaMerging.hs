{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Schema
  ( testSchema,
  )
where

import Control.Applicative (pure)
import Control.Monad ((>=>))
import Control.Monad.Fail (fail)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.Morpheus.Core
  ( parseFullGQLDocument,
    render,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resultOr,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import Prelude
  ( ($),
    (.),
    Eq (..),
    FilePath,
    IO,
    otherwise,
    show,
  )

readSource :: FilePath -> IO (Schema VALID)
readSource =
  L.readFile
    >=> (resultOr (fail . show) pure . parseFullGQLDocument)

readSchema1 :: FilePath -> IO (Schema VALID)
readSchema1 = readSource . (<> "/schema-1.gql")

readSchema2 :: FilePath -> IO (Schema VALID)
readSchema2 = readSource . (<> "/schema-2.gql")

readSchemaResult :: FilePath -> IO (Schema VALID)
readSchemaResult = readSource . (<> "/schema-result.gql")

schemaCase :: FilePath -> TestTree
schemaCase url = testCase url $ do
  schema1 <- readSchema1 url
  schema2 <- readSchema2 url
  expected <- readSchemaResult url
  assertion expected (schema1 <> schema2)

assertion :: Schema VALID -> Schema VALID -> IO ()
assertion expectedSchema schema
  | render expectedSchema == render schema = pure ()
  | otherwise =
    assertFailure
      $ unpack
      $ "expected: \n " <> render expectedSchema <> " \n but got: \n " <> render schema

testSchema :: TestTree
testSchema = schemaCase "merge/schema"
