{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Morpheus.Response
  ( assertResponse,
    getQuery,
    fromEither,
    expects,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Result (..),
    ToJSON (..),
    Value (..),
    decode,
    eitherDecode,
    fromJSON,
    object,
    (.=),
  )
import Relude hiding (ByteString)
import Test.Morpheus.File (FileUrl (fileName), readGQL, readJSON)
import Test.Morpheus.JSONDiff
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( testCase,
  )

data CaseAssertion a
  = OK
  | Expected a
  deriving (Generic, Eq)

instance (FromJSON a) => FromJSON (CaseAssertion a) where
  parseJSON (String "OK") = pure OK
  parseJSON v = Expected <$> parseJSON v

instance (ToJSON a) => ToJSON (CaseAssertion a) where
  toJSON OK = String "OK"
  toJSON (Expected v) = toJSON v

getResponse :: (FromJSON a) => FileUrl -> IO (CaseAssertion a)
getResponse = readJSON "response" >=> either fail pure . eitherDecode

assertResponse ::
  (FromJSON a, Eq a, ToJSON a) =>
  (FileUrl -> IO (CaseAssertion a)) ->
  FileUrl ->
  TestTree
assertResponse f url = testCase (fileName url) $ do
  actual <- f url
  expected <- getResponse url
  jsonEQ expected actual

runResult :: Result a -> IO a
runResult (Success x) = pure x
runResult (Error x) = fail x

getQuery :: (FromJSON req) => FileUrl -> IO req
getQuery url = do
  query <- String <$> readGQL "query" url
  variables <- decode <$> readJSON "variables" url <|> pure Nothing
  mkQuery query variables

mkQuery :: (FromJSON a) => Value -> Maybe Value -> IO a
mkQuery query variables =
  runResult
    $ fromJSON
    $ object
      [ "query" .= query,
        "variables" .= variables
      ]

fromEither :: (ToJSON err) => Either err a -> CaseAssertion err
fromEither (Left err) = Expected err
fromEither Right {} = OK

expects :: (ToJSON a) => a -> CaseAssertion Value
expects = Expected . toJSON
