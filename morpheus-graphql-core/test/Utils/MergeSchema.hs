{-# LANGUAGE DuplicateRecordFields #-}
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
  ( parseGQLDocument,
    render,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( resultOr,
  )
import Data.Morpheus.Types.Internal.Stitching (Stitching (stitch))
import Data.Semigroup ((<>))
import Data.Text (unpack)
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
readSchema =
  L.readFile . ("test/" <>) . (<> ".gql")
    >=> (resultOr (fail . show) pure . parseGQLDocument)

readSchemaResult :: FilePath -> IO (Schema VALID)
readSchemaResult = readSchema . (<> "/expected/ok")

schemaCase :: FilePath -> TestTree
schemaCase url = testCase url $ do
  schema <- readSchema (url <> "/test/schema")
  extension <- readSchema (url <> "/test/ext")
  result <- resultOr (fail . show) pure (stitch schema extension)
  expected <- readSchemaResult url
  assertion expected result

assertion :: Schema VALID -> Schema VALID -> IO ()
assertion expectedSchema schema
  | render expectedSchema == render schema = pure ()
  | otherwise =
    assertFailure
      $ unpack
      $ "expected: \n " <> render expectedSchema <> " \n but got: \n " <> render schema

test :: TestTree
test =
  testGroup
    "merge schema"
    [ schemaCase "merge/schema/simple-query",
      schemaCase "merge/schema/query-subscription-mutation"
    ]
