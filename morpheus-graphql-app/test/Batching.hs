{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Batching
  ( runBatchingTest,
  )
where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as M
import Data.Morpheus.App
  ( App (..),
    mkApp,
    runApp,
  )
import Data.Morpheus.App.Internal.Resolving (resultOr)
import Data.Morpheus.App.NamedResolvers
  ( Haxl,
    NamedArg,
    NamedResolverFunction,
    NamedResponse,
    RootResolverValue,
    State (ReqState),
    enum,
    getArgument,
    getIds,
    getResponseById,
    list,
    object,
    queryResolvers,
    ref,
    refs,
    withHaxl,
  )
import Data.Morpheus.Core
  ( parseSchema,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
  )
import Data.Morpheus.Types.Internal.AST (QUERY, Schema, TypeName, VALID, ValidValue, Value (..))
import Haxl.Core
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

type AppM = GenHaxl ()

withDeityHaxl :: AppM w b -> IO b
withDeityHaxl = withHaxl (ReqState resMap fetchDeityIds)

getNamedIds :: Haxl [NamedArg]
getNamedIds = getIds

getNamedResponseById :: NamedArg -> Haxl NamedResponse
getNamedResponseById = getResponseById

fetchDeityIds :: IO [NamedArg]
fetchDeityIds = do
  print ("Fetch Ids" :: String)
  pure [("", "Morpheus"), ("Zeus", Null), ("Ares", Null)]

type ResMap = M.Map TypeName ([ValidValue] -> IO [ValidValue])

resMap :: ResMap
resMap =
  M.fromList
    [ ("Deity", fetchDeityNames),
      ("Power", fetchDeityPowers)
    ]

fetchDeityNames :: [ValidValue] -> IO [ValidValue]
fetchDeityNames ids = do
  print ("Fetch Name for: " <> show ids)
  pure ids

fetchDeityPowers :: [ValidValue] -> IO [ValidValue]
fetchDeityPowers ids = do
  print ("Fetch Power for: " <> show ids)
  pure $ map (const "Shapeshifting") ids

-- DEITIES
resolverDeities :: Monad m => RootResolverValue e m
resolverDeities =
  queryResolvers
    [ ( "Query",
        traverse
          ( const $
              object
                [ ("deity", ref "Deity" <$> getArgument "id"),
                  ("deities", pure $ refs "Deity" ["zeus", "morpheus"])
                ]
          )
      ),
      ("Deity", deityResolver)
    ]

deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver = traverse deityRes
  where
    deityRes "zeus" =
      object
        [ ("name", pure "Zeus"),
          ("power", pure $ list [])
        ]
    deityRes _ =
      object
        [ ("name", pure "Morpheus"),
          ("power", pure $ list [enum "Shapeshifting"])
        ]

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

getApps :: FileUrl -> IO (App e IO)
getApps _ = do
  schemaDeities <- getSchema "test/named-resolvers/deities.gql"
  pure $ mkApp schemaDeities resolverDeities

runBatchingTest :: FileUrl -> FileUrl -> TestTree
runBatchingTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)
