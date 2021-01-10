{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Schema
  ( testSchema,
  )
where

import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode, object)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.App.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Morpheus.Core (parseFullGQLDocument)
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Schema,
    VALID,
  )
import Data.Text (pack)
import Relude hiding (ByteString, toString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Utils.Utils
  ( CaseTree (..),
    FileUrl (..),
    caseFailure,
    scanSchemaTests,
    toString,
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
assertion expected Success {} = caseFailure (encode expected) "OK"
assertion expected Failure {errors} =
  caseFailure
    (encode expected)
    (encode (Errors errors))
