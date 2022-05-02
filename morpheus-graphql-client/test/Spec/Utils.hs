{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Spec.Utils
  ( mockApi,
    path,
    assertFetch,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( Fetch (..),
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

getJSON :: FilePath -> ByteString -> IO ByteString
getJSON p _ = getFile (p <> ".json")

mockApi :: FilePath -> ByteString -> IO ByteString
mockApi p = getJSON (p <> "/response")

assertFetch ::
  ( Fetch a,
    FromJSON a,
    Eq a,
    Show a
  ) =>
  FilePath ->
  Maybe FilePath ->
  Args a ->
  a ->
  TestTree
assertFetch folder file args v =
  testCase display $ do
    response <- fetch (getJSON (folder <> "/" <> fileName)) args
    assertEqual ("Test " <> display) (Right v) response
  where
    fileName = fromMaybe "response" file
    display = fromMaybe folder file
