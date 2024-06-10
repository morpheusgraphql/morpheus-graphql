{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Execution
  ( runExecutionTest,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (readFile)
import Data.Morpheus.App (mkApp, runApp)
import Data.Morpheus.App.Internal.Resolving
  ( ObjectTypeResolver (..),
    Resolver,
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
  ( Msg (..),
    QUERY,
    Schema,
    VALID,
    ValidValue,
  )
import Relude hiding (ByteString, readFile)
import Test.Morpheus
  ( FileUrl,
    file,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

type ExecState m = (StateT Int m)

type ResQ m = Resolver QUERY () (ExecState m)

getName :: ValidValue -> ResolverValue m
getName "zeus" = "Zeus"
getName "morpheus" = "Morpheus"
getName "poseidon" = "Zeus"
getName "cronos" = "Cronos"
getName _ = ""

restrictExecutions :: (Monad m) => Int -> ResQ m ()
restrictExecutions expected = do
  count <- lift get
  if expected == count then pure () else throwError ("unexpected execution count. expected " <> msg expected <> " but got " <> msg count <> ".")

deityResolver :: (Monad m) => ValidValue -> ResQ m (ResolverValue (ResQ m))
deityResolver name = do
  lift (modify (+ 1))
  pure $ mkObject "Deity" [("name", pure $ getName name)]

resolvers :: (Monad m) => RootResolverValue () (ExecState m)
resolvers =
  RootResolverValue
    { queryResolver =
        pure
          ( ObjectTypeResolver
              $ fromList
                [ ("deity", (getArgument "id" >>= deityResolver) <* restrictExecutions 1),
                  ("deities", (list <$> traverse deityResolver ["zeus", "morpheus"]) <* restrictExecutions 2)
                ]
          ),
      mutationResolver = pure (ObjectTypeResolver mempty),
      subscriptionResolver = pure (ObjectTypeResolver mempty),
      channelMap = Nothing
    }

getSchema :: FileUrl -> IO (Schema VALID)
getSchema url = readFile (toString url) >>= resultOr (fail . show) pure . parseSchema

runExecutionTest :: FileUrl -> FileUrl -> TestTree
runExecutionTest url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = do
      schemaDeities <- getSchema (file url "schema.gql")
      (response, _) <- runStateT (runApp (mkApp schemaDeities resolvers) req) 0
      pure response
