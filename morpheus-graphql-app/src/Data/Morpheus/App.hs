{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App
  ( Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
    App (..),
    AppData (..),
    runApp,
    withDebugger,
    mkApp,
    runAppStream,
    MapAPI (..),
  )
where

import qualified Data.Aeson as A
import Data.Morpheus.App.Internal.Resolving
  ( ResolverContext (..),
    ResponseStream,
    ResultT (..),
    RootResolverValue,
    cleanEvents,
    resultOr,
    runRootResolverValue,
  )
import Data.Morpheus.App.Internal.Stitching (Stitching (..))
import Data.Morpheus.App.MapAPI (MapAPI (..))
import Data.Morpheus.App.SchemaAPI (withSystemFields)
import Data.Morpheus.Core
  ( Config (..),
    RenderGQL (..),
    VALIDATION_MODE (..),
    ValidateSchema (..),
    debugConfig,
    defaultConfig,
    parseRequestWith,
  )
import Data.Morpheus.Ext.SemigroupM ((<:>))
import Data.Morpheus.Internal.Utils
  ( empty,
    failure,
    prop,
  )
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLErrors,
    Operation (..),
    Schema (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    VALID,
    Value,
  )
import Relude hiding (empty)

mkApp :: ValidateSchema s => Schema s -> RootResolverValue e m -> App e m
mkApp appSchema appResolvers =
  resultOr
    FailApp
    (App . AppData defaultConfig appResolvers)
    (validateSchema True defaultConfig appSchema)

data App event (m :: * -> *)
  = App {app :: AppData event m VALID}
  | FailApp {appErrors :: GQLErrors}

instance RenderGQL (App e m) where
  renderGQL App {app} = renderGQL app
  renderGQL FailApp {appErrors} = renderGQL (A.encode appErrors)

instance Monad m => Semigroup (App e m) where
  (FailApp err1) <> (FailApp err2) = FailApp (err1 <> err2)
  FailApp {appErrors} <> App {} = FailApp appErrors
  App {} <> FailApp {appErrors} = FailApp appErrors
  (App x) <> (App y) = resultOr FailApp App (stitch x y)

data AppData event (m :: * -> *) s = AppData
  { appConfig :: Config,
    appResolvers :: RootResolverValue event m,
    appSchema :: Schema s
  }

instance RenderGQL (AppData e m s) where
  renderGQL = renderGQL . appSchema

instance Monad m => Stitching (AppData e m s) where
  stitch x y =
    AppData (appConfig y)
      <$> prop stitch appResolvers x y
      <*> prop stitch appSchema x y

runAppData ::
  (Monad m, ValidateSchema s) =>
  AppData event m s ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runAppData AppData {appConfig, appSchema, appResolvers} request = do
  validRequest <- validateReq appSchema appConfig request
  resolvers <- withSystemFields (schema validRequest) appResolvers
  runRootResolverValue resolvers validRequest

validateReq ::
  ( Monad m,
    ValidateSchema s
  ) =>
  Schema s ->
  Config ->
  GQLRequest ->
  ResponseStream event m ResolverContext
validateReq inputSchema config request = cleanEvents $ ResultT $ pure $ do
  validSchema <- validateSchema True config inputSchema
  schema <- internalSchema <:> validSchema
  operation <- parseRequestWith config validSchema request
  pure $
    ResolverContext
      { schema,
        config,
        operation,
        currentTypeName = "Root",
        currentSelection =
          Selection
            { selectionName = "Root",
              selectionArguments = empty,
              selectionPosition = operationPosition operation,
              selectionAlias = Nothing,
              selectionContent = SelectionSet (operationSelection operation),
              selectionDirectives = []
            }
      }

stateless ::
  Functor m =>
  ResponseStream event m (Value VALID) ->
  m GQLResponse
stateless = fmap renderResponse . runResultT

runAppStream :: Monad m => App event m -> GQLRequest -> ResponseStream event m (Value VALID)
runAppStream App {app} = runAppData app
runAppStream FailApp {appErrors} = const $ failure appErrors

runApp :: (MapAPI a b, Monad m) => App e m -> a -> m b
runApp app = mapAPI (stateless . runAppStream app)

withDebugger :: App e m -> App e m
withDebugger App {app = AppData {appConfig = Config {..}, ..}} =
  App {app = AppData {appConfig = Config {debug = True, ..}, ..}, ..}
withDebugger x = x
