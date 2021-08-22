{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RelationalResolving
  ( test,
  )
where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Morpheus.App
  ( App (..),
    mkApp,
    runApp,
  )
import Data.Morpheus.App.Internal.Resolving
  ( RelTypeResolver,
    RootResolverValue (..),
    getArgument,
    mkList,
    mkNull,
    mkString,
    mkTypeResolverRef,
    mkTypeResolvers,
    resultOr,
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
    ScalarValue (String),
    Schema,
    VALID,
    Value (..),
  )
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    deepScan,
    mkUrl,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

-- DEITIES
resolverDeities :: Monad m => RootResolverValue e m
resolverDeities =
  TypeResolversValue
    { queryTypeResolvers =
        mkTypeResolvers
          [ ( "Query",
              const
                [ ( "deity",
                    mkTypeResolverRef "Deity"
                      <$> getArgument "id"
                  )
                ]
            ),
            ("Deity", deityResolver)
          ],
      mutationTypeResolvers = Nothing,
      subscriptionTypeResolvers = Nothing,
      typeResolverChannels = Nothing
    }

deityResolver :: Monad m => RelTypeResolver QUERY e m
deityResolver (Scalar (String "zeus")) =
  [ ("name", pure $ mkString "Zeus"),
    ("power", pure $ mkList [])
  ]
deityResolver _ =
  [ ("name", pure $ mkString "Morpheus"),
    ("power", pure $ mkList [mkString "Shapeshifting"])
  ]

-- REALMS
resolverRealms :: Monad m => RootResolverValue e m
resolverRealms =
  TypeResolversValue
    { queryTypeResolvers =
        mkTypeResolvers
          [ ( "Query",
              const
                [ ( "realm",
                    mkTypeResolverRef "Realm"
                      <$> getArgument "id"
                  )
                ]
            ),
            ("Deity", deityResolverExt),
            ("Realm", realmResolver)
          ],
      mutationTypeResolvers = Nothing,
      subscriptionTypeResolvers = Nothing,
      typeResolverChannels = Nothing
    }

deityResolverExt :: Monad m => RelTypeResolver QUERY e m
deityResolverExt (Scalar (String "zeus")) =
  [ ("realm", pure $ mkTypeResolverRef "Realm" $ Scalar $ String "olympus")
  ]
deityResolverExt _ =
  [("realm", pure mkNull)]

realmResolver :: Monad m => RelTypeResolver QUERY e m
realmResolver (Scalar (String "olympus")) =
  [ ("name", pure $ mkString "Mount Olympus"),
    ("owner", pure $ mkTypeResolverRef "Deity" $ Scalar (String "zeus"))
  ]
realmResolver _ =
  [ ("name", pure $ mkString "None")
  ]

getApps :: FileUrl -> IO (App e IO)
getApps _ = do
  schemaDeities <- getSchema "test/relational/deities.gql"
  schemaRealms <- getSchema "test/relational/realms.gql"
  pure $
    mkApp schemaDeities resolverDeities
      <> mkApp schemaRealms resolverRealms

testQuery :: FileUrl -> FileUrl -> TestTree
testQuery url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = getApps url >>= (`runApp` req)

runApiTest :: FileUrl -> [FileUrl] -> [TestTree]
runApiTest url = map (testQuery url)

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

test :: IO TestTree
test = deepScan runApiTest (mkUrl "relational")
