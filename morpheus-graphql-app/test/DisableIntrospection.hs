{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DisableIntrospection
  ( runNamedDisableIntrospectionTest,
  )
where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Morpheus.App
  ( App (..),
    mkApp,
    runApp,
  )
import Data.Morpheus.App.Internal.Resolving (resultOr)
import Data.Morpheus.App.NamedResolvers
  ( NamedResolverFunction,
    RootResolverValue,
    enum,
    getArgument,
    list,
    object,
    queryResolvers,
    ref,
    refs,
    variant,
  )
import Data.Morpheus.Core
  ( parseSchema,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( QUERY,
    Schema,
    VALID,
  )
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

-- REALMS
namedQuery :: (Monad m) => RootResolverValue e m
namedQuery = queryResolvers [("Query", traverse (const $ object [("name", pure "some text")]))]

resolveQuery :: (Monad m) => RootResolverValue e m
resolveQuery = queryResolvers [("Query", traverse (const $ object [("name", pure "some text")]))]

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

runNamedDisableIntrospectionTest :: FileUrl -> FileUrl -> TestTree
runNamedDisableIntrospectionTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)
    where 
      app = do
        schemaRealms <- getSchema "test/disable-introspection/realms.gql"
        pure (mkApp schemaDeities namedQuery)

runDisableIntrospectionTest :: FileUrl -> FileUrl -> TestTree
runDisableIntrospectionTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)
    where 
      app = do
        schemaRealms <- getSchema "test/disable-introspection/realms.gql"
        pure (mkApp schemaDeities resolveQuery)

