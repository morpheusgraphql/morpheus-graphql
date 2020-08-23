{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Api
  ( Api,
    runApiWith,
    runApi,
    debugApi,
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

data Api event (m :: * -> *)
  = Api {app :: App event m VALID}
  | FailApi {appErrors :: GQLErrors}

instance Semigroup (Api e m) where
  (FailApi err1) <> (FailApi err2) = FailApi (err1 <> err2)
  FailApi {appErrors} <> Api {} = FailApi appErrors
  Api {} <> FailApi {appErrors} = FailApi appErrors
  (Api x) <> (Api y) = resultOr FailApi Api (stitch x y)

data App event (m :: * -> *) s = App
  { appResolvers :: RootResModel event m,
    appSchema :: Schema s
  }

instance Stitching (App e m s) where
  stitch x y =
    App
      <$> prop stitch appResolvers x y
      <*> prop stitch appSchema x y

runApi :: Monad m => Api event m -> GQLRequest -> ResponseStream event m (Value VALID)
runApi api = runApiWith api defaultConfig

debugApi :: Monad m => Api event m -> GQLRequest -> ResponseStream event m (Value VALID)
debugApi app = runApiWith app debugConfig

runApiWith :: Monad m => Api event m -> Config -> GQLRequest -> ResponseStream event m (Value VALID)
runApiWith Api {app} = runAppNode app
runApiWith FailApi {appErrors} = const $ const $ failure appErrors

runAppNode ::
  (Monad m, ValidateSchema s) =>
  App event m s ->
  Config ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runAppNode App {appSchema, appResolvers} config request = do
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
