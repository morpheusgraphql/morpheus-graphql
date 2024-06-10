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
    disableIntrospection,
    mkApp,
    runAppStream,
    MapAPI (..),
    eitherSchema,
    withConstraint,
    APIConstraint,
  )
where

import Control.Monad.Except (throwError)
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.App.Internal.Resolving
  ( ResolverContext (..),
    ResponseStream,
    ResultT (..),
    RootResolverValue,
    resultOr,
    runRootResolverValue,
  )
import Data.Morpheus.App.Internal.Stitching (Stitching (..))
import Data.Morpheus.App.MapAPI (MapAPI (..))
import Data.Morpheus.Core
  ( Config (..),
    RenderGQL (..),
    VALIDATION_MODE (..),
    ValidateSchema (..),
    debugConfig,
    defaultConfig,
    internalSchema,
    parseRequestWith,
    render,
  )
import Data.Morpheus.Internal.Ext (GQLResult, (<:>))
import Data.Morpheus.Internal.Utils
  ( empty,
    prop,
    throwErrors,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    GQLErrors,
    Operation (..),
    OperationType (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    VALID,
    Value,
    toAny,
  )
import qualified Data.Morpheus.Types.Internal.AST as AST
import Relude hiding (ByteString, empty)

mkApp :: (ValidateSchema s) => Schema s -> RootResolverValue e m -> App e m
mkApp appSchema appResolvers =
  resultOr
    FailApp
    (App . AppData defaultConfig [] appResolvers)
    (validateSchema True defaultConfig appSchema)

data App event (m :: Type -> Type)
  = App {app :: AppData event m VALID}
  | FailApp {appErrors :: GQLErrors}

instance RenderGQL (App e m) where
  renderGQL App {app} = renderGQL app
  renderGQL FailApp {appErrors} = renderGQL $ A.encode $ toList appErrors

instance (Monad m) => Semigroup (App e m) where
  (FailApp err1) <> (FailApp err2) = FailApp (err1 <> err2)
  FailApp {appErrors} <> App {} = FailApp appErrors
  App {} <> FailApp {appErrors} = FailApp appErrors
  (App x) <> (App y) = resultOr FailApp App (stitch x y)

type APIConstraint = Schema VALID -> Operation VALID -> Either String ()

data AppData event (m :: Type -> Type) s = AppData
  { appConfig :: Config,
    constraints :: [APIConstraint],
    appResolvers :: RootResolverValue event m,
    appSchema :: Schema s
  }

instance RenderGQL (AppData e m s) where
  renderGQL = renderGQL . appSchema

instance (Monad m) => Stitching (AppData e m s) where
  stitch x y =
    AppData
      (appConfig y)
      (constraints x <> constraints y)
      <$> prop stitch appResolvers x y
      <*> prop stitch appSchema x y

checkConstraints :: Schema VALID -> Operation VALID -> [APIConstraint] -> GQLResult ()
checkConstraints appSchema validRequest constraints =
  either
    (throwError . fromString . ("API Constraint: " <>))
    (const $ pure ())
    (traverse (\f -> f appSchema validRequest) constraints)

runAppData ::
  (Monad m, ValidateSchema s) =>
  AppData event m s ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runAppData AppData {appConfig, appSchema, appResolvers, constraints} request = do
  validRequest <- validateReq constraints appSchema appConfig request
  runRootResolverValue appResolvers validRequest

validateReq ::
  ( Monad m,
    ValidateSchema s
  ) =>
  [APIConstraint] ->
  Schema s ->
  Config ->
  GQLRequest ->
  ResponseStream event m ResolverContext
validateReq constraints inputSchema config request = ResultT
  $ pure
  $ do
    validSchema <- validateSchema True config inputSchema
    schema <- internalSchema <:> validSchema
    operation <- parseRequestWith config validSchema request
    checkConstraints schema operation constraints
    pure
      ( [],
        ResolverContext
          { schema,
            config,
            operation,
            currentType =
              toAny
                $ fromMaybe
                  (AST.query schema)
                  (rootType (operationType operation) schema),
            currentSelection =
              Selection
                { selectionName = "Root",
                  selectionArguments = empty,
                  selectionPosition = operationPosition operation,
                  selectionAlias = Nothing,
                  selectionContent = SelectionSet (operationSelection operation),
                  selectionDirectives = empty,
                  selectionOrigin = Nothing
                }
          }
      )

rootType :: OperationType -> Schema s -> Maybe (AST.TypeDefinition AST.OBJECT s)
rootType OPERATION_QUERY = Just . AST.query
rootType OPERATION_MUTATION = mutation
rootType OPERATION_SUBSCRIPTION = subscription

stateless ::
  (Functor m) =>
  ResponseStream event m (Value VALID) ->
  m GQLResponse
stateless = fmap (renderResponse . fmap snd) . runResultT

runAppStream :: (Monad m) => App event m -> GQLRequest -> ResponseStream event m (Value VALID)
runAppStream App {app} = runAppData app
runAppStream FailApp {appErrors} = const $ throwErrors appErrors

runApp :: (MapAPI a b, Monad m) => App e m -> a -> m b
runApp app = mapAPI (stateless . runAppStream app)

mapApp :: (AppData e m VALID -> AppData e m VALID) -> App e m -> App e m
mapApp f App {app} =
  App {app = f app}
mapApp _ x = x

mapConfig :: (Config -> Config) -> App e m -> App e m
mapConfig g = mapApp f
  where
    f AppData {appConfig, ..} = AppData {appConfig = g appConfig, ..}

withDebugger :: App e m -> App e m
withDebugger = mapConfig (\c -> c {debug = True})

disableIntrospection :: App e m -> App e m
disableIntrospection = mapConfig (\c -> c {introspection = False})

withConstraint :: APIConstraint -> App e m -> App e m
withConstraint constraint = mapApp f
  where
    f AppData {..} = AppData {constraints = constraint : constraints, ..}

eitherSchema :: App event m -> Either [GQLError] ByteString
eitherSchema (App AppData {appSchema}) = Right (render appSchema)
eitherSchema (FailApp errors) = Left (toList errors)
