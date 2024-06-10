{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Morpheus
  ( FileUrl,
    mkUrl,
    cd,
    file,
    deepScan,
    scan,
    -- get app
    getAppsBy,
    -- tests
    testApi,
    testSchema,
    testQueryRendering,
    renderingAssertion,
    testQuery,
    testQueryValidation,
    -- main
    mainTest,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    encode,
  )
import Data.ByteString.Lazy.Char8 (ByteString)
import Relude hiding (ByteString)
import Test.Morpheus.File
import Test.Morpheus.Response
import Test.Morpheus.Utils
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )

mainTest :: String -> [IO TestTree] -> IO ()
mainTest name xs = do
  tests <- sequence xs
  defaultMain
    $ testGroup
      name
      tests

--
testApi ::
  (FromJSON req, ToJSON res) =>
  (req -> IO res) ->
  FileUrl ->
  TestTree
testApi api =
  assertResponse
    (fmap expects . api <=< getQuery)

testQuery ::
  (FromJSON req, ToJSON err, Eq err, FromJSON err) =>
  (req -> Either err a) ->
  FileUrl ->
  TestTree
testQuery f =
  assertResponse
    (fmap (fromEither . f) . getQuery)

testSchema ::
  (ToJSON err, Eq err, FromJSON err) =>
  (ByteString -> Either err a) ->
  FileUrl ->
  TestTree
testSchema f =
  assertResponse
    (fmap (fromEither . f) . readSchemaFile)

renderingAssertion ::
  (ToJSON e) =>
  (FileUrl -> IO (Either e ByteString)) ->
  FileUrl ->
  TestTree
renderingAssertion api url = testCase (fileName url) $ do
  actual <- api url
  expected <- readGQL "rendering" url
  either
    (assertFailure . (" error: " <>) . show . encode)
    (requireEq id expected)
    actual

getAppBy ::
  (Show err, FromJSON resolvers) =>
  ( ByteString -> Either err schema,
    schema -> resolvers -> app
  ) ->
  FileUrl ->
  IO app
getAppBy (parseSchema, mkApp) url =
  mkApp
    <$> getSchema parseSchema url
    <*> getResolver url

testQueryValidation ::
  ( ToJSON errors,
    Show errors,
    FromJSON request,
    FromJSON errors,
    Eq errors
  ) =>
  ( schema -> request -> Either errors a,
    ByteString -> Either errors schema
  ) ->
  FileUrl ->
  FileUrl ->
  TestTree
testQueryValidation (parseRequest, parseSchema) schemaUrl =
  assertResponse
    ( \url -> do
        schema <- getSchema parseSchema schemaUrl
        query <- getQuery url
        pure $ fromEither (parseRequest schema query)
    )

testQueryRendering ::
  ( ToJSON errors,
    Show errors,
    FromJSON request
  ) =>
  ( schema -> request -> Either errors ByteString,
    ByteString -> Either errors schema
  ) ->
  FileUrl ->
  FileUrl ->
  TestTree
testQueryRendering (parseRequest, parseSchema) schemaUrl =
  renderingAssertion
    ( \requestUrl -> do
        request <- getQuery requestUrl
        schema <- getSchema parseSchema schemaUrl
        pure $ parseRequest schema request
    )

getAppsWIth ::
  (Semigroup b, Show err, FromJSON resolvers) =>
  ( ByteString -> Either err schema,
    schema -> resolvers -> b
  ) ->
  FileUrl ->
  [FilePath] ->
  IO b
getAppsWIth f url [] = getAppBy f url
getAppsWIth f url (x : xs) = sconcat <$> traverse (getAppBy f . file url) (x :| xs)

getAppsBy ::
  (Semigroup b, Show err, FromJSON resolvers) =>
  ( ByteString -> Either err schema,
    schema -> resolvers -> b
  ) ->
  FileUrl ->
  IO b
getAppsBy f url = do
  files <- searchAppFiles url
  getAppsWIth f url (sort files)
