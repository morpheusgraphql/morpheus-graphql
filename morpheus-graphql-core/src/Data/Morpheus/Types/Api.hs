{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Api
  ( Api (..),
    ApiRunner (..),
    runApiWith,
    mkApi,
    App (..),
  )
where

import Data.Morpheus.Internal.Utils
  ( (<:>),
    empty,
    failure,
    prop,
  )
import Data.Morpheus.Parser
  ( parseRequestWith,
  )
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Schema.SchemaAPI (withSystemFields)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    GQLResponse,
    MapAPI (..),
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
import Data.Morpheus.Types.Internal.Config
  ( Config (..),
    debugConfig,
    defaultConfig,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( ResolverContext (..),
    ResponseStream,
    ResultT (..),
    RootResModel,
    cleanEvents,
    resultOr,
    runRootResModel,
  )
import Data.Morpheus.Types.Internal.Stitching (Stitching (..))
import Data.Morpheus.Validation.Document.Validation (ValidateSchema (..))

mkApi :: ValidateSchema s => Schema s -> RootResModel e m -> Api e m
mkApi appSchema appResolvers =
  resultOr
    FailApi
    (Api . App appResolvers)
    (validateSchema True defaultConfig appSchema)

data Api event (m :: * -> *)
  = Api {app :: App event m VALID}
  | FailApi {appErrors :: GQLErrors}

instance Monad m => Semigroup (Api e m) where
  (FailApi err1) <> (FailApi err2) = FailApi (err1 <> err2)
  FailApi {appErrors} <> Api {} = FailApi appErrors
  Api {} <> FailApi {appErrors} = FailApi appErrors
  (Api x) <> (Api y) = resultOr FailApi Api (stitch x y)

data App event (m :: * -> *) s = App
  { appResolvers :: RootResModel event m,
    appSchema :: Schema s
  }

instance Monad m => Stitching (App e m s) where
  stitch x y =
    App
      <$> prop stitch appResolvers x y
      <*> prop stitch appSchema x y

runApiWith :: Monad m => Api event m -> Config -> GQLRequest -> ResponseStream event m (Value VALID)
runApiWith Api {app} = runApp app
runApiWith FailApi {appErrors} = const $ const $ failure appErrors

runApp ::
  (Monad m, ValidateSchema s) =>
  App event m s ->
  Config ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runApp App {appSchema, appResolvers} config request = do
  validRequest <- validateReq appSchema config request
  resovers <- withSystemFields (schema validRequest) appResolvers
  runRootResModel resovers validRequest

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
  operation <- parseRequestWith config schema request
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

class Monad m => ApiRunner e (m :: * -> *) a b where
  runApi :: Api e m -> a -> b
  debugApi :: Api e m -> a -> b

instance Monad m => ApiRunner e m GQLRequest (m GQLResponse) where
  runApi api = stateless . runApiWith api defaultConfig
  debugApi api = stateless . runApiWith api debugConfig

instance (Monad m, MapAPI a a) => ApiRunner e m a (m a) where
  runApi app = mapAPI (runApi app)
  debugApi app = mapAPI (debugApi app)
