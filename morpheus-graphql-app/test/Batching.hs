{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Batching
  ( runBatchingTest,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.HashSet (fromList)
import Data.Map (lookup)
import Data.Morpheus.App
  ( App (..),
    mkApp,
    runApp,
  )
import Data.Morpheus.App.Internal.Resolving (ResolverValue, resultOr)
import Data.Morpheus.App.NamedResolvers
  ( NamedResolverFunction,
    RootResolverValue,
    enum,
    getArgument,
    list,
    nullRes,
    object,
    queryResolvers,
    ref,
    refs,
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
    TypeName,
    VALID,
    ValidValue,
    Value (..),
    msg,
    unpackName,
  )
import Relude hiding (ByteString, fromList)
import Test.Morpheus
  ( FileUrl,
    file,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

type BatchedValues = (HashSet ValidValue)

type BatchingConstraints = Map Text BatchedValues

typeConstraint :: (Monad m) => BatchingConstraints -> (TypeName, NamedResolverFunction QUERY e m) -> (TypeName, NamedResolverFunction QUERY e m)
typeConstraint cons (name, f) = (name,) $ maybe f (require f) (lookup (unpackName name) cons)

require :: (Monad m) => NamedResolverFunction QUERY e m -> BatchedValues -> NamedResolverFunction QUERY e m
require f req args
  | fromList args == req = f args
  | otherwise = throwError ("was not batched! expected: " <> msg (List $ toList req) <> "got: " <> msg (List args))

gods :: [ValidValue]
gods = ["poseidon", "morpheus", "zeus"]

getName :: ValidValue -> ResolverValue m
getName "zeus" = "Zeus"
getName "morpheus" = "Morpheus"
getName "poseidon" = "Zeus"
getName "cronos" = "Cronos"
getName _ = ""

getPowers :: ValidValue -> [ResolverValue m]
getPowers "zeus" = [enum "Thunderbolt"]
getPowers "morpheus" = [enum "Shapeshifting"]
getPowers _ = []

deityResolver :: (Monad m) => NamedResolverFunction QUERY e m
deityResolver = traverse getDeity
  where
    getDeity name
      | name `elem` gods =
          object
            [ ("name", pure $ getName name),
              ("power", pure $ list $ getPowers name)
            ]
      | otherwise = nullRes

resolveQuery :: (Monad m) => NamedResolverFunction QUERY e m
resolveQuery = traverse getQuery
  where
    getQuery _ =
      object
        [ ("deity", ref "Deity" <$> getArgument "id"),
          ("deities", pure $ refs "Deity" ["zeus", "morpheus"])
        ]

resolvers :: (Monad m) => BatchingConstraints -> RootResolverValue e m
resolvers cons =
  queryResolvers
    $ typeConstraint cons
    <$> [ ("Query", resolveQuery),
          ("Deity", deityResolver)
        ]

getSchema :: FileUrl -> IO (Schema VALID)
getSchema url = LBS.readFile (toString url) >>= resultOr (fail . show) pure . parseSchema

getBatchingConstraint :: FileUrl -> IO BatchingConstraints
getBatchingConstraint url = LBS.readFile (toString (file url "batching.json")) >>= either (fail . show) pure . eitherDecode

getApps :: BatchingConstraints -> FileUrl -> IO (App e IO)
getApps con x = do
  schemaDeities <- getSchema (file x "schema.gql")
  pure $ mkApp schemaDeities (resolvers con)

runBatchingTest :: FileUrl -> FileUrl -> TestTree
runBatchingTest url fileUrl = testApi api fileUrl
  where
    api :: GQLRequest -> IO GQLResponse
    api req = do
      constraints <- getBatchingConstraint fileUrl
      getApps constraints url >>= (`runApp` req)
