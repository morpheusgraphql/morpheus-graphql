{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Utils
  ( path,
    assertFetch,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
  )
import Relude hiding (ByteString, exp)
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

path :: FilePath -> FilePath
path name = "test/Case/" <> name

getFile :: FilePath -> IO ByteString
getFile p = L.readFile (path p)

mockJSON :: FilePath -> ByteString -> IO ByteString
mockJSON p _ = getFile (p <> ".json")

assertFetch ::
  ( Fetch a,
    FromJSON a,
    Eq a,
    Show a
  ) =>
  FilePath ->
  Maybe FilePath ->
  Args a ->
  Either (FetchError a) a ->
  TestTree
assertFetch folder file args v =
  testCase display $ do
    response <- fetch (mockJSON (folder <> "/" <> fileName)) args
    assertEqual ("Test " <> display) v response
  where
    fileName = fromMaybe "response" file
    display = fromMaybe folder file
