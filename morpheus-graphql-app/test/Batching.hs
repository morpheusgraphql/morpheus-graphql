{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Batching
  ( runBatchingTest,
  )
where

import Data.ByteString.Lazy.Char8 (unpack)
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
  )
import Data.Morpheus.Core
  ( parseSchema,
    render,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
  )
import Data.Morpheus.Types.Internal.AST (QUERY, Schema, VALID)
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

-- DEITIES

debugArgs :: String -> NamedResolverFunction QUERY e m -> NamedResolverFunction QUERY e m
debugArgs name f args = trace (name <> ":: " <> intercalate ", " (map (unpack . render) args)) (f args)

deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver = debugArgs "DEITY" (traverse getDeity)
  where
    getDeity "zeus" =
      object
        [ ("name", pure "Zeus"),
          ("power", pure $ list [])
        ]
    getDeity _ =
      object
        [ ("name", pure "Morpheus"),
          ("power", pure $ list [enum "Shapeshifting"])
        ]

resolveQuery :: Monad m => NamedResolverFunction QUERY e m
resolveQuery = debugArgs "QUERY" (traverse _resolveQuery)
  where
    _resolveQuery _ =
      object
        [ ("deity", ref "Deity" <$> getArgument "id"),
          ("deities", pure $ refs "Deity" ["zeus", "morpheus"])
        ]

resolvers :: Monad m => RootResolverValue e m
resolvers =
  queryResolvers
    [ ("Query", resolveQuery),
      ("Deity", deityResolver)
    ]

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

getApps :: FileUrl -> IO (App e IO)
getApps _ = do
  schemaDeities <- getSchema "test/named-resolvers/deities.gql"
  pure $ mkApp schemaDeities resolvers

runBatchingTest :: FileUrl -> FileUrl -> TestTree
runBatchingTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)