{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.MergeSchema
  ( test,
  )
where

import Control.Applicative (pure)
import Control.Monad ((>=>))
import Control.Monad.Fail (fail)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.Morpheus.Core
  ( Api (..),
    App (..),
    mkApi,
    parseGQLDocument,
    render,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resultOr,
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Lib
  ( getResolver,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Prelude
  ( ($),
    (.),
    Eq (..),
    FilePath,
    IO,
    otherwise,
    show,
  )

readSchema :: FilePath -> IO (Schema VALID)
readSchema = L.readFile >=> (resultOr (fail . show) pure . parseGQLDocument)

loadApi :: FilePath -> IO (Api () IO)
loadApi url = do
  schema <- readSchema ("test/" <> url <> ".gql")
  resolvers <- getResolver ("test/" <> url <> ".json")
  pure $ mkApi schema resolvers

schemaCase :: FilePath -> TestTree
schemaCase url = testCase url $ do
  schema <- loadApi (url <> "/api/app")
  extension <- loadApi (url <> "/api/ext")
  expected <- readSchema ("test/" <> url <> "/expected/ok.gql")
  assertion expected (schema <> extension)

assertion :: Schema VALID -> Api () IO -> IO ()
assertion expectedSchema (Api App {appSchema})
  | render expectedSchema == render appSchema = pure ()
  | otherwise =
    assertFailure
      $ unpack
      $ "expected: \n " <> render expectedSchema <> " \n but got: \n " <> render appSchema
assertion _ (FailApi gqlerror) = assertFailure $ " error: " <> show gqlerror

test :: TestTree
test =
  testGroup
    "merge schema"
    [ schemaCase "merge/schema/simple-query",
      schemaCase "merge/schema/query-subscription-mutation"
    ]
