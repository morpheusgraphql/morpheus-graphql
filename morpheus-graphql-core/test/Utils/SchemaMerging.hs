{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Schema
  ( testSchema,
  )
where

import Control.Applicative (pure)
import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode, object)
import qualified Data.ByteString.Lazy as L (readFile)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor (fmap)
import Data.Morpheus.Core (parseFullGQLDocument)
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Schema,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import Data.Text (pack)
import GHC.Generics (Generic)
import Lib
  ( CaseTree (..),
    FileUrl (..),
    scanSchemaTests,
    toString,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Prelude
  ( ($),
    (.),
    Eq (..),
    FilePath,
    IO,
    String,
    id,
    show,
  )

readSource :: FilePath -> IO ByteString
readSource = L.readFile

readSchema1 :: FilePath -> IO (Eventless (Schema VALID))
readSchema1 = fmap parseFullGQLDocument . readSource . (<> "/schema-1.gql")

readSchema2 :: FilePath -> IO (Eventless (Schema VALID))
readSchema2 = fmap parseFullGQLDocument . readSource . (<> "/schema-2.gql")

readSchemaResult :: FilePath -> IO (Eventless (Schema VALID))
readSchemaResult = fmap parseFullGQLDocument . readSource . (<> "/schema-2.gql")

schemaCase :: FilePath -> TestTree
schemaCase url = testCase url $ do
  schema1 <- readSchema1 url
  schema2 <- readSchema1 url
  expected <- readSchemaResult url
  assertion expected (schema1 <> schema2)

assertion :: Schema VALID -> Eventless (Schema VALID) -> IO ()
assertion schema Success {result}
  | schema == result = pure ()
  | otherwise = LB.unpack ("expected: \n " <> render expected <> " \n but got: \n OK")
assertion expected Failure {errors} =
  assertFailure $
    LB.unpack
      ("expected: \n " <> encode expected <> " \n but got: \n " <> encode (Errors errors))

testSchema = schemaCase "merge/schema"
