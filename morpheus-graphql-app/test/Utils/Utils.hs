{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Utils
  ( expectedResponse,
    getRequest,
    assertValidSchema,
    getResolvers,
    getResolver,
    assertion,
    testRequest,
  )
where

import qualified Data.Aeson as A
  ( Value (..),
    decode,
    encode,
  )
import qualified Data.ByteString.Lazy as L (readFile)
import Data.HashMap.Lazy (lookup)
import Data.Morpheus.App
  ( App (..),
    runAppStream,
  )
import Data.Morpheus.App.Internal.Resolving
  ( ResolverValue,
    ResponseStream,
    ResultT (..),
    RootResolverValue (..),
    mkNull,
    mkValue,
    resultOr,
  )
import Data.Morpheus.Core (parseSchema)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema (..),
    VALID,
    Value,
  )
import Relude hiding (ByteString)
import Test.Morpheus.Utils
  ( FileUrl (..),
    ReadSource (..),
    assertEqualFailure,
    assertValidSchemaFailure,
    getVariables,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( testCase,
  )

assertion :: A.Value -> ResponseStream e Identity (Value VALID) -> IO ()
assertion expected (ResultT (Identity actual))
  | Just expected == A.decode actualValue = pure ()
  | otherwise = assertEqualFailure (A.encode expected) actualValue
  where
    actualValue = A.encode (renderResponse actual)

assertValidSchema :: FileUrl -> IO (Schema VALID)
assertValidSchema =
  readSchemaFile
    >=> resultOr assertValidSchemaFailure pure . parseSchema

expectedResponse :: FileUrl -> IO A.Value
expectedResponse p = fromMaybe A.Null . A.decode <$> readResponseFile p

getRequest :: FileUrl -> IO GQLRequest
getRequest p =
  GQLRequest
    Nothing
    <$> readQueryFile p
    <*> getVariables p

getResolvers :: Monad m => FileUrl -> IO (RootResolverValue e m)
getResolvers p = getResolver (toString p <> "/resolvers.json")

testRequest :: IO (App e Identity) -> FileUrl -> TestTree
testRequest apiMonad path = testCase (fileName path) $ do
  api <- apiMonad
  actual <- runAppStream api <$> getRequest path
  expected <- expectedResponse path
  assertion expected actual

getResolver :: Monad m => FilePath -> IO (RootResolverValue e m)
getResolver p = do
  res <- fromMaybe A.Null . A.decode <$> L.readFile p
  pure
    RootResolverValue
      { query = pure (lookupRes "query" res),
        mutation = pure (lookupRes "mutation" res),
        subscription = pure (lookupRes "subscription" res),
        channelMap = Nothing
      }

lookupRes ::
  ( Monad m
  ) =>
  Text ->
  A.Value ->
  ResolverValue m
lookupRes name (A.Object fields) = maybe mkNull mkValue (lookup name fields)
lookupRes _ _ = mkNull
