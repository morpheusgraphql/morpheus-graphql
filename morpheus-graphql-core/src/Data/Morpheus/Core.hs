{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Core
  ( runApi,
    parseDSL,
    parseFullGQLDocument,
    parseGQLDocument,
    parseTypeSystemDefinition,
    parseTypeDefinitions,
    validateRequest,
    parseRequestWith,
    validateSchema,
    parseRequest,
    RenderGQL (..),
    SelectionTree (..),
    Config (..),
    VALIDATION_MODE (..),
    defaultConfig,
    debugConfig,
    runAppWithConfig,
    runApp,
    debugApp,
    App (..),
  )
where

-- MORPHEUS
import Control.Monad ((>=>))
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Internal.Utils
  ( (<:>),
    empty,
    failure,
    prop,
  )
import Data.Morpheus.Parser
  ( parseRequest,
    parseRequestWith,
    parseTypeDefinitions,
    parseTypeSystemDefinition,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Schema.SchemaAPI (withSystemFields)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    GQLErrors,
    Operation (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    VALID,
    Value,
  )
import Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
    debugConfig,
    defaultConfig,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Channel,
    Eventless,
    ResolverContext (..),
    ResponseStream,
    ResultT (..),
    RootResModel,
    cleanEvents,
    resultOr,
    runRootResModel,
  )
import Data.Morpheus.Types.Internal.Stitching (Stitching (..))
import Data.Morpheus.Types.SelectionTree (SelectionTree (..))
import Data.Morpheus.Validation.Document.Validation (ValidateSchema (..))
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import qualified Data.Text.Lazy as LT
  ( toStrict,
  )
import Data.Text.Lazy.Encoding (decodeUtf8)

data App event (m :: * -> *)
  = App
      { appResolvers :: RootResModel event m,
        appSchema :: Schema CONST
      }
  | AppFailure
      { appErrors :: GQLErrors
      }

instance Eq (Channel e) => Semigroup (App e m) where
  (AppFailure err1) <> (AppFailure err2) = AppFailure (err1 <> err2)
  (AppFailure err) <> App {} = AppFailure err
  App {} <> (AppFailure err) = AppFailure err
  x@App {} <> y@App {} =
    resultOr
      AppFailure
      id
      ( App
          <$> prop stitch appResolvers x y
          <*> prop stitch appSchema x y
      )

runAppWithConfig :: Monad m => App event m -> Config -> GQLRequest -> ResponseStream event m (Value VALID)
runAppWithConfig App {appSchema, appResolvers} = runApi appSchema appResolvers
runAppWithConfig AppFailure {appErrors} = const $ const $ failure appErrors

runApp :: Monad m => App event m -> GQLRequest -> ResponseStream event m (Value VALID)
runApp app = runAppWithConfig app defaultConfig

debugApp :: Monad m => App event m -> GQLRequest -> ResponseStream event m (Value VALID)
debugApp app = runAppWithConfig app debugConfig

runApi ::
  forall event m s.
  (Monad m, ValidateSchema s) =>
  Schema s ->
  RootResModel event m ->
  Config ->
  GQLRequest ->
  ResponseStream event m (Value VALID)
runApi inputSchema resModel config request = do
  validRequest <- validateReq inputSchema config request
  resovers <- withSystemFields (schema validRequest) resModel
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

parseDSL :: ByteString -> Either String (Schema VALID)
parseDSL = resultOr (Left . show) pure . parseGQLDocument

parseGQLDocument :: ByteString -> Eventless (Schema VALID)
parseGQLDocument = parseTypeSystemDefinition . LT.toStrict . decodeUtf8

parseFullGQLDocument :: ByteString -> Eventless (Schema VALID)
parseFullGQLDocument = parseGQLDocument >=> (internalSchema <:>)
