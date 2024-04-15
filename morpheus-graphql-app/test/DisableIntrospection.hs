{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DisableIntrospection
  ( runNamedDisableIntrospectionTest,
    runDisableIntrospectionTest,
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
    getAppsBy,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

-- REALMS
getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

runNamedDisableIntrospectionTest :: FileUrl -> FileUrl -> TestTree
runNamedDisableIntrospectionTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = app url >>= (`runApp` req)
    app = do
      schemaRealms <- getSchema "test/disable-introspection/realms.gql"
      let resolvers = queryResolvers [("Query", traverse (const $ object [("name", pure "some text")]))]
      pure $ mkApp schemaDeities resolvers

runDisableIntrospectionTest :: FileUrl -> FileUrl -> TestTree
runDisableIntrospectionTest = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getAppsBy (toEither . parseSchema, mkApp) (mkUrl "disable-introspection") >>= (`runApp` req)