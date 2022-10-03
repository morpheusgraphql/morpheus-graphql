{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module APIConstraints
  ( runAPIConstraints,
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
  ( RootResolverValue,
    object,
    queryResolvers,
  )
import Data.Morpheus.Core
  ( parseSchema,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
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

resolvers :: Monad m => RootResolverValue e m
resolvers =
  queryResolvers
    [ ( "Query",
        const $
          object
            [ ("success", pure "success"),
              ("forbidden", pure "forbidden!")
            ]
      )
    ]

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

getApps :: FileUrl -> IO (App e IO)
getApps _ = (`mkApp` resolvers) <$> getSchema "test/api-constraints/schema.gql"

runAPIConstraints :: FileUrl -> FileUrl -> TestTree
runAPIConstraints url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)
