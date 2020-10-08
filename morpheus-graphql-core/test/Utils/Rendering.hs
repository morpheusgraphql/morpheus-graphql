{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.Rendering
  ( test,
  )
where

import Control.Applicative (pure)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Functor ((<$>), fmap)
import Data.Morpheus.Core
  ( defaultConfig,
    parseRequestWith,
    render,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    Operation,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Utils.Utils
  ( assertValidSchema,
    getRequest,
    readSource,
  )
import Prelude
  ( ($),
    (.),
    (==),
    IO,
    otherwise,
    show,
  )

assertion :: ByteString -> Eventless (Operation VALID) -> IO ()
assertion expected Success {result = actual}
  | expected == actualValue = pure ()
  | otherwise =
    assertFailure
      ("expected: \n\n " <> show expected <> " \n\n but got: \n\n " <> show actualValue)
  where
    actualValue = render actual
assertion _ Failure {errors} = assertFailure $ LB.unpack (encode errors)

test :: FieldName -> [FieldName] -> TestTree
test apiPath requestPath =
  testGroup (T.unpack $ readName apiPath) $
    fmap (testRendering apiPath) requestPath

getExpectedRendering :: FieldName -> IO ByteString
getExpectedRendering = readSource . (<> "/rendering.gql")

testRendering ::
  FieldName ->
  FieldName ->
  TestTree
testRendering apiPath path = testCase (T.unpack $ readName path) $ do
  schema <- assertValidSchema apiPath
  let fullPath = apiPath <> "/" <> path
  actual <- parseRequestWith defaultConfig schema <$> getRequest fullPath
  expected <- getExpectedRendering fullPath
  assertion expected actual
