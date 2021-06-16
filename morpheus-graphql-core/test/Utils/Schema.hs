{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Schema
  ( runSchemaTest,
  )
where

import Data.Aeson
  ( (.:),
    (.=),
    FromJSON (..),
    ToJSON (..),
    Value (..),
    eitherDecode,
    encode,
    object,
  )
import Data.Morpheus.Core (parseFullSchema)
import Data.Morpheus.Internal.Ext
  ( Eventless,
    Result (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Schema,
    VALID,
  )
import Data.Text (pack)
import Relude hiding (ByteString)
import Test.Morpheus.Utils
  ( FileUrl (..),
    ReadSource (..),
    assertEqualFailure,
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

readResponse :: ToString t => t -> IO Response
readResponse = fmap (either AesonError id . eitherDecode) . readResponseFile

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

runSchemaTest :: FileUrl -> [FileUrl] -> [TestTree]
runSchemaTest url _ =
  [ testCase (fileName url) $ do
      schema <- parseFullSchema <$> readSchemaFile url
      expected <- readResponse url
      assertion expected schema
  ]

assertion :: Response -> Eventless (Schema VALID) -> IO ()
assertion OK Success {} = pure ()
assertion Errors {errors = err} Failure {errors}
  | err == errors =
    pure
      ()
assertion expected Success {} = assertEqualFailure (encode expected) "OK"
assertion expected Failure {errors} =
  assertEqualFailure
    (encode expected)
    (encode (Errors errors))
