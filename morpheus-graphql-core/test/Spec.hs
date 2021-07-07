{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Data.Morpheus.Core
  ( defaultConfig,
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
    deepScan,
    mainTest,
    mkUrl,
    scan,
    testQueryRendering,
    testSchema,
  )
import Test.Tasty
  ( TestTree,
  )

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
      second render . toEither
        . parseRequestWith defaultConfig schema

main :: IO ()
main =
  mainTest
    "Core tests"
    [ scan runSchemaTest (mkUrl "schema"),
      deepScan runRenderingTest (mkUrl "rendering")
    ]
