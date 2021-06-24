{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Data.Aeson as A
  ( Value (..),
  )
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.App
  ( App (..),
    AppData (..),
    mkApp,
    runAppStream,
  )
import Data.Morpheus.App.Internal.Resolving
  ( ResponseStream,
    ResultT (..),
    RootResolverValue (..),
    mkValue,
    resultOr,
  )
import Data.Morpheus.Core
  ( parseSchema,
    render,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    VALID,
    VALID,
    Value,
  )
import Relude hiding (ByteString)
import Test.Morpheus.Utils
  ( FileUrl (..),
    deepScan,
    getResolverValues,
    getSchema,
    renderingAssertion,
    runCaseTree,
    select,
    testApi,
  )
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )

toResponse :: ResponseStream e IO (Value VALID) -> IO GQLResponse
toResponse (ResultT actual) = renderResponse <$> actual

runAPI :: IO (App e IO) -> GQLRequest -> IO GQLResponse
runAPI api req = api >>= toResponse . (`runAppStream` req)

testRequest :: IO (App e IO) -> FileUrl -> TestTree
testRequest api = testApi (runAPI api)

mkRootResolver :: Monad m => (A.Value, A.Value, A.Value) -> RootResolverValue e m
mkRootResolver (q, m, s) =
  RootResolverValue
    { query = pure (mkValue q),
      mutation = pure (mkValue m),
      subscription = pure (mkValue s),
      channelMap = Nothing
    }

loadApi :: FileUrl -> IO (App () IO)
loadApi url = do
  schema <- getSchema (resultOr Left Right . parseSchema) url
  resolvers <- mkRootResolver <$> getResolverValues url
  pure $ mkApp schema resolvers

loadMergedApis :: FileUrl -> IO (App () IO)
loadMergedApis url = do
  schema <- loadApi (select url "app")
  extension <- loadApi (select url "ext")
  pure (schema <> extension)

toSchema :: App event m -> Either GQLErrors ByteString
toSchema (App AppData {appSchema}) = Right (render appSchema)
toSchema (FailApp errors) = Left errors

runMergeTest :: FileUrl -> [FileUrl] -> [TestTree]
runMergeTest url assets =
  renderingAssertion (fmap toSchema . loadMergedApis) url
    : map (testRequest (loadMergedApis url)) assets

runApiTest :: FileUrl -> [FileUrl] -> [TestTree]
runApiTest url = map (testRequest (loadApi url))

main :: IO ()
main = do
  mergeTests <- runCaseTree runMergeTest <$> deepScan "merge"
  apiTests <- runCaseTree runApiTest <$> deepScan "api"
  defaultMain $
    testGroup
      "App Tests"
      [ mergeTests,
        apiTests
      ]
