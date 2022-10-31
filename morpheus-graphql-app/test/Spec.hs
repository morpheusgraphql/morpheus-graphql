{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import APIConstraints (runAPIConstraints)
import Batching (runBatchingTest)
import Data.Morpheus.App
  ( App (..),
    eitherSchema,
    mkApp,
    runApp,
  )
import Data.Morpheus.Core
  ( parseSchema,
  )
import Data.Morpheus.Internal.Ext
  ( toEither,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
  )
import NamedResolvers (runNamedResolversTest)
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    deepScan,
    getAppsBy,
    mainTest,
    mkUrl,
    renderingAssertion,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

getApps :: FileUrl -> IO (App e IO)
getApps = getAppsBy (toEither . parseSchema, mkApp)

testQuery :: FileUrl -> FileUrl -> TestTree
testQuery url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)

runApiTest :: FileUrl -> [FileUrl] -> [TestTree]
runApiTest url = map (testQuery url)

runRenderingTest :: FileUrl -> TestTree
runRenderingTest = renderingAssertion (fmap eitherSchema . getApps)

runMergeTest :: FileUrl -> [FileUrl] -> [TestTree]
runMergeTest url assets = runRenderingTest url : runApiTest url assets

main :: IO ()
main =
  mainTest
    "App Tests"
    [ deepScan runMergeTest (mkUrl "merge"),
      deepScan runApiTest (mkUrl "api"),
      deepScan (map . runNamedResolversTest) (mkUrl "named-resolvers"),
      deepScan (map . runAPIConstraints) (mkUrl "api-constraints"),
      deepScan (map . runBatchingTest) (mkUrl "batching")
    ]
