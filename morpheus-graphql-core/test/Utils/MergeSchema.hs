{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.MergeSchema
  ( test,
  )
where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Monad.Fail (fail)
import Data.Aeson (decode, encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L (readFile)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import Data.Functor ((<$>), fmap)
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Core
  ( App (..),
    AppData (..),
    defaultConfig,
    mkApp,
    mkApp,
    parseGQLDocument,
    render,
    runAppWith,
  )
import Data.Morpheus.Types.IO
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    Schema,
    VALID,
    Value,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResponseStream,
    ResultT (..),
    resultOr,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Data.Traversable (traverse)
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Utils.Utils
  ( expectedResponse,
    getRequest,
    getResolver,
  )
import Prelude
  ( ($),
    (.),
    Eq (..),
    IO,
    Maybe (..),
    otherwise,
    show,
  )

readSchema :: FieldName -> IO (Schema VALID)
readSchema (FieldName p) = L.readFile (unpack p) >>= (resultOr (fail . show) pure . parseGQLDocument)

loadApi :: FieldName -> IO (App () Identity)
loadApi url = do
  schema <- readSchema ("test/" <> url <> ".gql")
  resolvers <- getResolver ("test/" <> url <> ".json")
  pure $ mkApp schema resolvers

schemaAssertion :: App () Identity -> Schema VALID -> IO ()
schemaAssertion (App AppData {appSchema}) expectedSchema
  | render expectedSchema == render appSchema = pure ()
  | otherwise =
    assertFailure
      $ unpack
      $ "expected: \n " <> render expectedSchema <> " \n but got: \n " <> render appSchema
schemaAssertion (FailApp gqlerror) _ = assertFailure $ " error: " <> show gqlerror

schemaCase :: (FieldName, [FieldName]) -> IO TestTree
schemaCase (url, files) = do
  schema <- loadApi (url <> "/api/app")
  extension <- loadApi (url <> "/api/ext")
  let api = schema <> extension
  pure $
    testGroup
      (show url)
      [ testCase
          "Schema"
          (readSchema ("test/" <> url <> "/expected/ok.gql") >>= schemaAssertion api),
        testGroup
          "Requests"
          (fmap (testApiRequest api url) files)
      ]

test :: IO TestTree
test =
  testGroup
    "merge schema"
    <$> traverse
      schemaCase
      [ ("merge/schema/simple-query", ["query"]),
        ("merge/schema/query-subscription-mutation", ["query", "mutation"])
      ]

assertion :: A.Value -> ResponseStream e Identity (Value VALID) -> IO ()
assertion expected (ResultT (Identity actual))
  | Just expected == decode actualValue = pure ()
  | otherwise =
    assertFailure $
      LB.unpack
        ("expected: \n\n " <> encode expected <> " \n\n but got: \n\n " <> actualValue)
  where
    actualValue = encode (renderResponse actual)

testApiRequest ::
  App () Identity ->
  FieldName ->
  FieldName ->
  TestTree
testApiRequest api base path = testCase (unpack $ readName path) $ do
  let fullPath = base <> "/request/" <> path
  actual <- runAppWith api defaultConfig <$> getRequest fullPath
  expected <- expectedResponse fullPath
  assertion expected actual
