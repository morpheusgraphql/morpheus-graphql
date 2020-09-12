{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Rendering
  ( test,
  )
where

import Control.Applicative (pure)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Functor ((<$>), fmap)
import Data.Morpheus.Core
  ( defaultConfig,
    parseRequestWith,
    renderGQL,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    Operation,
    Token,
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
  )

assertion :: Token -> Eventless (Operation VALID) -> IO ()
assertion expected Success {result = actual}
  | expected == renderGQL actualValue = pure ()
  | otherwise =
    assertFailure $
      T.unpack
        ("expected: \n\n " <> expected <> " \n\n but got: \n\n " <> actualValue)
  where
    actualValue = renderGQL actual
assertion _ Failure {errors} = assertFailure $ LB.unpack (encode errors)

test :: FieldName -> [FieldName] -> TestTree
test apiPath requestPath =
  testGroup (T.unpack $ readName apiPath) $
    fmap (testRendering apiPath) requestPath

getExpectedRendering :: FieldName -> IO Token
getExpectedRendering = fmap (T.pack . LB.unpack) . readSource . (<> "/rendering.gql")

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
