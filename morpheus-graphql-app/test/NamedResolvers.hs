{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NamedResolvers
  ( runNamedResolversTest,
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
    getArgument,
    list,
    ref,
    object,
    queryResolvers, 
    variant,
    refs,
    enum
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

-- DEITIES
resolverDeities :: Monad m => RootResolverValue e m
resolverDeities = queryResolvers
    [ ("Query", const $ object 
        [( "deity", ref "Deity" <$> getArgument "id"),
         ("deities", pure $ refs "Deity" ["zeus","morpheus"] )
        ]
      ),
      ("Deity", deityResolver)
    ]

deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver "zeus" = object
  [ ("name", pure "Zeus"),
    ("power", pure $ list ["Thunderbolt"])
  ]
deityResolver _ = object
  [ ("name", pure "Morpheus"),
    ("power", pure $ list [enum "Shapeshifting"])
  ]

-- REALMS
resolverRealms :: Monad m => RootResolverValue e m
resolverRealms = queryResolvers
    [ ("Query", const $ object [
        ( "realm", ref "Realm" <$> getArgument "id" ),
        ( "realms", pure $ refs "Realm" ["olympus","dreams"] )
        ]
      ),
      ("Deity", deityResolverExt),
      ("Realm", realmResolver)
    ]

deityResolverExt :: Monad m => NamedResolverFunction QUERY e m
deityResolverExt  "zeus" = object [("realm", pure $ ref "Realm"  "olympus")]
deityResolverExt  "morpheus" = object [("realm", pure $ ref "Realm"  "dreams")]
deityResolverExt _ = object []

realmResolver :: Monad m => NamedResolverFunction QUERY e m
realmResolver "olympus" = object
  [ ("name", pure  "Mount Olympus"),
    ("owner", pure $ ref "Deity"  "zeus")
  ]
realmResolver "dreams" = object
  [ ("name", pure  "Fictional world of dreams"),
    ("owner", pure $ ref "Deity"  "morpheus")
  ]
realmResolver _ = object
  [ ("name", pure  "None")
  ]

-- ENTITIES
resolverEntities :: Monad m => RootResolverValue e m
resolverEntities = queryResolvers
    [ ("Query", const $ object [
      ( "entity", ref "Entity" <$> getArgument "id" ),
      ( "entities", pure $ refs "Entity"  
        ["zeus", "morpheus", "olympus", "dreams"] )
      ]),
      ("Entity", resolveEntity)
    ]

resolveEntity :: Monad m => NamedResolverFunction QUERY e m
resolveEntity  "zeus" = variant "Deity"   "zeus"
resolveEntity  "morpheus" = variant "Deity"   "morpheus"
resolveEntity  "olympus" = variant "Realm"  "olympus"
resolveEntity  "dreams" = variant "Realm"  "dreams"
resolveEntity _ = object []


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

runNamedResolversTest :: FileUrl -> FileUrl -> TestTree
runNamedResolversTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)


