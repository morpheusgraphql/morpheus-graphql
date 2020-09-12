{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Validator
  ( apiTest,
    assertion,
  )
where

import Control.Applicative (pure)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe (..))
import Data.Morpheus.Core
  ( defaultConfig,
    render,
    validateRequest,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    Operation,
    Token,
    VALID,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    ResponseStream,
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
    expectedResponse,
    getQuery,
    getRequest,
    getResolvers,
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
  | expected == render actualValue = pure ()
  | otherwise =
    assertFailure $
      T.unpack
        ("expected: \n\n " <> expected <> " \n\n but got: \n\n " <> actualValue)
  where
    actualValue = render actual
assertion _ _ = assertFailure "TODO:"

apiTest :: FieldName -> [FieldName] -> TestTree
apiTest apiPath requestPath =
  testGroup (T.unpack $ readName apiPath) $
    fmap (testApiRequest apiPath) requestPath

testApiRequest ::
  FieldName ->
  FieldName ->
  TestTree
testApiRequest apiPath path = testCase (T.unpack $ readName path) $ do
  schema <- assertValidSchema apiPath
  let fullPath = apiPath <> "/" <> path
  actual <- validateRequest defaultConfig schema <$> getQuery fullPath
  expected <- T.pack . LB.unpack <$> readSource fullPath
  assertion expected actual
