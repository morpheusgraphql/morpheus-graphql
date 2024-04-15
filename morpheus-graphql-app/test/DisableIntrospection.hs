{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DisableIntrospection
  ( runNamedDisableIntrospectionTest,
    runDisableIntrospectionTest,
  )
where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Morpheus.App (mkApp, runApp)
import Data.Morpheus.App.Internal.Resolving (resultOr)
import Data.Morpheus.App.NamedResolvers (object, queryResolvers)
import Data.Morpheus.Core (parseSchema)
import Data.Morpheus.Internal.Ext (toEither)
import Data.Morpheus.Types.IO (GQLRequest (..), GQLResponse)
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    file,
    getAppsBy,
    testApi,
  )
import Test.Tasty (TestTree)

runNamedDisableIntrospectionTest :: FileUrl -> FileUrl -> TestTree
runNamedDisableIntrospectionTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = do
      schema <- LBS.readFile (toString (file url "schema.gql")) >>= resultOr (fail . show) pure . parseSchema
      let resolvers = queryResolvers [("Query", traverse (const $ object [("name", pure "Morpheus")]))]
      let app = mkApp schema resolvers
      runApp app req

runDisableIntrospectionTest :: FileUrl -> FileUrl -> TestTree
runDisableIntrospectionTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getAppsBy (toEither . parseSchema, mkApp) url >>= (`runApp` req)