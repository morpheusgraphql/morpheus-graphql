{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Execution
  ( runExecutionTest,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Morpheus.App (mkApp, runApp)
import Data.Morpheus.App.Internal.Resolving
  ( ObjectTypeResolver (..),
    ResolverValue,
    RootResolverValue (..),
    mkObject,
    resultOr,
  )
import Data.Morpheus.App.NamedResolvers
  ( getArgument,
    list,
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
    ValidValue,
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

getName :: ValidValue -> ResolverValue m
getName "zeus" = "Zeus"
getName "morpheus" = "Morpheus"
getName "poseidon" = "Zeus"
getName "cronos" = "Cronos"
getName _ = ""

-- TODO: restrict execution count
deityResolver :: Monad m => ValidValue -> m (ResolverValue m)
deityResolver name = pure $ mkObject "Deity" [("name", pure $ getName name)]

resolvers :: Monad m => RootResolverValue e m
resolvers =
  RootResolverValue
    { queryResolver =
        pure
          ( ObjectTypeResolver $
              fromList
                [ ("deity", getArgument "id" >>= deityResolver),
                  ("deities", list <$> traverse deityResolver ["zeus", "morpheus"])
                ]
          ),
      mutationResolver = pure (ObjectTypeResolver mempty),
      subscriptionResolver = pure (ObjectTypeResolver mempty),
      channelMap = Nothing
    }

getSchema :: FileUrl -> IO (Schema VALID)
getSchema url = LBS.readFile (toString url) >>= resultOr (fail . show) pure . parseSchema

runExecutionTest :: FileUrl -> FileUrl -> TestTree
runExecutionTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = do
      schemaDeities <- getSchema (file url "schema.gql")
      runApp (mkApp schemaDeities resolvers) req
