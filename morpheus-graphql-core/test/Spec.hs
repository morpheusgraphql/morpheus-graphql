{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Core
  ( defaultConfig,
    parseRequestWith,
    parseSchema,
    render,
  )
import Data.Morpheus.Internal.Ext (resultOr)
import Relude hiding (ByteString)
import Test.Morpheus.Utils
  ( FileUrl (..),
    deepScan,
    foldCaseTree,
    getRequest,
    getSchema,
    readResponse,
    readSchemaFile,
    renderingAssertion,
    responseAssertion,
    runCaseTree,
  )
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit (testCase)

runSchemaTest :: FileUrl -> TestTree
runSchemaTest url =
  testCase (fileName url) $ do
    schema <- resultOr Left Right . parseSchema <$> readSchemaFile url
    expected <- readResponse url
    responseAssertion expected schema

renderRequest :: FileUrl -> FileUrl -> IO (Either ByteString ByteString)
renderRequest schemaUrl url = do
  schema <- getSchema (resultOr Left Right . parseSchema) schemaUrl
  resultOr (Left . encode) (Right . render)
    . parseRequestWith defaultConfig schema
    <$> getRequest url

runRenderingTest :: FileUrl -> [FileUrl] -> [TestTree]
runRenderingTest url = map (renderingAssertion (renderRequest url))

main :: IO ()
main =
  sequence
    [ foldCaseTree runSchemaTest <$> deepScan "schema",
      runCaseTree runRenderingTest <$> deepScan "rendering"
    ]
    >>= defaultMain . testGroup "core tests"
