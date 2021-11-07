{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Morpheus.Core
  ( defaultConfig,
    parseRequest,
    parseRequestWith,
    parseSchema,
    render,
  )
import Data.Morpheus.Internal.Ext
  ( toEither,
  )
import Data.Morpheus.Types.IO (GQLRequest)
import Data.Morpheus.Types.Internal.AST (GQLError, Schema, VALID)
import Relude ((.), Either, Functor (fmap), IO, NonEmpty, map)
import Test.Morpheus
  ( FileUrl,
    cd,
    deepScan,
    mainTest,
    mkUrl,
    scan,
    testQuery,
    testQueryRendering,
    testQueryValidation,
    testSchema,
  )
import Test.Tasty
  ( TestTree,
  )

runQueryTest :: FileUrl -> TestTree
runQueryTest = testQuery (toEither . parseRequest)

runSchemaTest :: FileUrl -> TestTree
runSchemaTest = testSchema (toEither . parseSchema)

runRenderingTest :: FileUrl -> [FileUrl] -> [TestTree]
runRenderingTest url = map (testQueryRendering (parseQuery, toEither . parseSchema) url)

runQueryValidationTest :: FileUrl -> [FileUrl] -> [TestTree]
runQueryValidationTest url = map (testQueryValidation (parseQuery, toEither . parseSchema) url)

parseQuery :: Schema VALID -> GQLRequest -> Either (NonEmpty GQLError) ByteString
parseQuery schema =
  toEither . fmap render
    . parseRequestWith defaultConfig schema

main :: IO ()
main =
  mainTest
    "Core tests"
    [ scan runQueryTest (cd (mkUrl "query") "parsing"),
      scan runSchemaTest (mkUrl "schema"),
      deepScan runQueryValidationTest (cd (mkUrl "query") "validation"),
      deepScan runRenderingTest (mkUrl "rendering")
    ]
