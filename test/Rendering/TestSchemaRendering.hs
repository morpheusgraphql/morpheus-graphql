{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Rendering.TestSchemaRendering
  ( testSchemaRendering,
  )
where

import Data.ByteString.Lazy.Char8 (readFile)
import Data.Morpheus.Document (toGraphQLDocument)
import Rendering.Schema (path, proxy)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Prelude (($))

testSchemaRendering :: TestTree
testSchemaRendering = testCase "Test Rendering" $ do
  let schema = toGraphQLDocument proxy
  expected <- readFile path
  assertEqual "test schema Rendering" expected schema
