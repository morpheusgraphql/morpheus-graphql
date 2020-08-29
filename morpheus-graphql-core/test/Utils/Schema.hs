{-# LANGUAGE DeriveGeneric #-}
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
import Data.Either (Either (..), either)
import Data.Functor ((<$>), fmap)
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Utils.Utils
  ( CaseTree (..),
    FileUrl (..),
    scanSchemaTests,
    toString,
  )
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

readSchema :: FilePath -> IO (Eventless (Schema VALID))
readSchema = fmap parseFullGQLDocument . readSource . (<> "/schema.gql")

readResponse :: FilePath -> IO Response
readResponse = fmap (either AesonError id . eitherDecode) . readSource . (<> "/response.json")

data Response
  = OK
  | Errors {errors :: GQLErrors}
  | AesonError String
  deriving (Generic)

instance FromJSON Response where
  parseJSON (Object v) =
    Errors <$> v .: "errors"
  parseJSON (String "OK") = pure OK
  parseJSON v = pure $ AesonError (show v)

instance ToJSON Response where
  toJSON OK = String "OK"
  toJSON (Errors err) = object ["errors" .= toJSON err]
  toJSON (AesonError err) = String (pack err)

toTests :: CaseTree -> TestTree
toTests CaseTree {caseUrl, children = Left {}} = schemaCase caseUrl
toTests CaseTree {caseUrl = FileUrl {fileName}, children = Right children} =
  testGroup
    fileName
    (fmap toTests children)

testSchema :: IO TestTree
testSchema = toTests <$> scanSchemaTests "test/schema"

schemaCase :: FileUrl -> TestTree
schemaCase url = testCase (fileName url) $ do
  schema <- readSchema (toString url)
  expected <- readResponse (toString url)
  assertion expected schema

assertion :: Response -> Eventless (Schema VALID) -> IO ()
assertion OK Success {} = pure ()
assertion Errors {errors = err} Failure {errors}
  | err == errors =
    pure
      ()
assertion expected Success {} =
  assertFailure $
    LB.unpack
      ("expected: \n " <> encode expected <> " \n but got: \n OK")
assertion expected Failure {errors} =
  assertFailure $
    LB.unpack
      ("expected: \n " <> encode expected <> " \n but got: \n " <> encode (Errors errors))
