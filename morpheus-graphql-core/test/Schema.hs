{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema
  ( testSchema,
  )
where

import Control.Monad ((<=<))
import Data.Aeson ((.:), (.=), FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode, object)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import Data.Either (either)
import Data.Morpheus.Core (parseFullGQLDocument, validateSchema)
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Name,
    Schema,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import Data.Text (pack)
import GHC.Generics (Generic)
import Lib (readSource)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

readSchema :: Name -> IO (Eventless Schema)
readSchema = fmap (validateSchema <=< parseFullGQLDocument) . readSource . ("schema/" <>) . (<> "/schema.gql")

readResponse :: Name -> IO Response
readResponse = fmap (either AesonError id . eitherDecode) . readSource . ("schema/" <>) . (<> "/response.json")

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

testSchema :: TestTree
testSchema =
  testGroup
    "Test Schema"
    [ testGroup
        "Validation"
        $ map
          (uncurry schemaCase)
          [ ("validation/interface/ok", "interface validation success"),
            ("validation/interface/fail", "interface validation fails")
          ]
    ]

schemaCase :: Name -> String -> TestTree
schemaCase path description = testCase description $ do
  schema <- readSchema path
  expected <- readResponse path
  assertion expected schema

assertion :: Response -> Eventless Schema -> IO ()
assertion OK Success {} = return ()
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
