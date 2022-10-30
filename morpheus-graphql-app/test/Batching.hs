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
    variant,
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
import Haxl.Core
  ( GenHaxl,
  )
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

withDeityHaxl :: GenHaxl () w b -> IO b
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

-- REALMS
resolverRealms :: Monad m => RootResolverValue e m
resolverRealms =
  queryResolvers
    [ ( "Query",
        traverse
          ( const $
              object
                [ ("realm", ref "Realm" <$> getArgument "id"),
                  ("realms", pure $ refs "Realm" ["olympus", "dreams"])
                ]
          )
      ),
      ("Deity", deityResolverExt),
      ("Realm", realmResolver)
    ]

deityResolverExt :: Monad m => NamedResolverFunction QUERY e m
deityResolverExt = traverse deityExt
  where
    deityExt "zeus" = object [("realm", pure $ ref "Realm" "olympus")]
    deityExt "morpheus" = object [("realm", pure $ ref "Realm" "dreams")]
    deityExt _ = object []

realmResolver :: Monad m => NamedResolverFunction QUERY e m
realmResolver = traverse realmResolver'
  where
    realmResolver' "olympus" =
      object
        [ ("name", pure "Mount Olympus"),
          ("owner", pure $ ref "Deity" "zeus")
        ]
    realmResolver' "dreams" =
      object
        [ ("name", pure "Fictional world of dreams"),
          ("owner", pure $ ref "Deity" "morpheus")
        ]
    realmResolver' _ =
      object
        [ ("name", pure "None")
        ]

-- ENTITIES
resolverEntities :: Monad m => RootResolverValue e m
resolverEntities =
  queryResolvers
    [ ( "Query",
        traverse
          ( const $
              object
                [ ("entity", ref "Entity" <$> getArgument "id"),
                  ( "entities",
                    pure $
                      refs
                        "Entity"
                        ["zeus", "morpheus", "olympus", "dreams"]
                  )
                ]
          )
      ),
      ("Entity", resolveEntity)
    ]

resolveEntity :: Monad m => NamedResolverFunction QUERY e m
resolveEntity = traverse resEntity
  where
    resEntity "zeus" = variant "Deity" "zeus"
    resEntity "morpheus" = variant "Deity" "morpheus"
    resEntity "olympus" = variant "Realm" "olympus"
    resEntity "dreams" = variant "Realm" "dreams"
    resEntity _ = object []

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

getApps :: FileUrl -> IO (App e IO)
getApps _ = do
  schemaDeities <- getSchema "test/named-resolvers/deities.gql"
  schemaRealms <- getSchema "test/named-resolvers/realms.gql"
  schemaEntities <- getSchema "test/named-resolvers/entities.gql"
  pure $
    mkApp schemaDeities resolverDeities
      <> mkApp schemaRealms resolverRealms
      <> mkApp schemaEntities resolverEntities

runBatchingTest :: FileUrl -> FileUrl -> TestTree
runBatchingTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)
