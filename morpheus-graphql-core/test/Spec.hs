{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

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
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    cd,
    deepScan,
    mainTest,
    mkUrl,
    queryValidation,
    scan,
    testQuery,
    testQueryRendering,
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
runRenderingTest url =
  map
    ( testQueryRendering
        (parseQuery, toEither . parseSchema)
        url
    )
  where
    parseQuery schema =
      toEither . fmap render
        . parseRequestWith defaultConfig schema

runQueryValidationTest :: FileUrl -> [FileUrl] -> [TestTree]
runQueryValidationTest url = map (queryValidation (parseQuery, toEither . parseSchema) url)

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
