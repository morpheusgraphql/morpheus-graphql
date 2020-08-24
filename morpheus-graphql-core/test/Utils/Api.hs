{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Api
  ( apiTest,
    assertion,
  )
where

import Control.Applicative (pure)
import qualified Data.Aeson as A
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LB (unpack)
import Data.Functor ((<$>), fmap)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Core (mkApp, runAppWith)
import Data.Morpheus.Types.IO
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    VALID,
    Value (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResponseStream,
    ResultT (..),
  )
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
import Utils.Utils
  ( assertValidSchema,
    expectedResponse,
    getRequest,
    getResolvers,
  )
import Prelude
  ( ($),
    (==),
    IO,
    otherwise,
  )

assertion :: A.Value -> ResponseStream e Identity (Value VALID) -> IO ()
assertion expected (ResultT (Identity actual))
  | Just expected == decode actualValue = pure ()
  | otherwise =
    assertFailure $
      LB.unpack
        ("expected: \n\n " <> encode expected <> " \n\n but got: \n\n " <> actualValue)
  where
    actualValue = encode (renderResponse actual)

apiTest :: FieldName -> [FieldName] -> TestTree
apiTest apiPath requestPath =
  testGroup (unpack $ readName apiPath) $
    fmap (testApiRequest apiPath) requestPath

testApiRequest ::
  FieldName ->
  FieldName ->
  TestTree
testApiRequest apiPath path = testCase (unpack $ readName path) $ do
  schema <- assertValidSchema apiPath
  resolvers <- getResolvers apiPath
  let fullPath = apiPath <> "/" <> path
  let api = mkApp schema resolvers
  actual <- runAppWith api <$> getRequest fullPath
  expected <- expectedResponse fullPath
  assertion expected actual
