{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Rendering.TestSchemaRendering
  ( testSchemaRendering
  ) where

import           Data.Morpheus.Document     (toGraphQLDocument)
import           Rendering.Schema           (schemaProxy)
import           Test.Tasty                 (TestTree)
import           Test.Tasty.HUnit           (assertEqual, testCase)

-- TODO: better Test
testSchemaRendering :: TestTree
testSchemaRendering = testCase "Test Rendering" $ assertEqual "test schema Rendering" expected schema
  where
    schema = toGraphQLDocument schemaProxy
    expected =
        "type Query { \n  user: User!\n  testUnion: TestUnion\n}\n\nenum TestEnum { \n  EnumA\n  EnumB\n  EnumC\n}\n\ntype Address { \n  street: [[[[String!]!]!]]\n}\n\ninput Coordinates { \n  latitude: TestScalar!\n  longitude: Int!\n}\n\ntype User { \n  type: String!\n  address(coordinates: Coordinates!, type: String): Int!\n  friend(id: ID!, cityID: TestEnum): User!\n}\n\nunion TestUnion =\n    User\n  | Address\n\nscalar TestScalar"