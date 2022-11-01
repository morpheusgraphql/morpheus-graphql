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
import Data.Map
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
    unpackName,
  )
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    file,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

-- DEITIES
type BatchingConstraints = Map Text [ValidValue]

typeConstraint :: Monad m => BatchingConstraints -> (TypeName, NamedResolverFunction QUERY e m) -> (TypeName, NamedResolverFunction QUERY e m)
typeConstraint cons (name, f) = (name,) $ maybe f (require f) (lookup (unpackName name) cons)

require :: Monad m => NamedResolverFunction QUERY e m -> [ValidValue] -> NamedResolverFunction QUERY e m
require f req args
  | args == req = f args
  | otherwise = throwError ("was not batched" <> show args)

deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver = traverse getDeity
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
resolveQuery = traverse _resolveQuery
  where
    _resolveQuery _ =
      object
        [ ("deity", ref "Deity" <$> getArgument "id"),
          ("deities", pure $ refs "Deity" ["zeus", "morpheus"])
        ]

resolvers :: Monad m => BatchingConstraints -> RootResolverValue e m
resolvers cons =
  queryResolvers $
    typeConstraint cons
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
