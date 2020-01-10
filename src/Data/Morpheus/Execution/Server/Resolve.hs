{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Morpheus.Execution.Server.Resolve
  ( statelessResolver
  , byteStringIO
  , streamResolver
  , statefulResolver
  , RootResCon
  , fullSchema
  , coreResolver
  )
where

import           Data.Semigroup                 ((<>))
import           Data.Aeson                     ( encode )
import           Data.Aeson.Internal            ( formatError
                                                , ifromJSON
                                                )
import           Data.Aeson.Parser              ( eitherDecodeWith
                                                , jsonNoDup
                                                )
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Proxy                     ( Proxy(..) )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( badRequestError )
import           Data.Morpheus.Execution.Server.Encode
                                                ( EncodeCon
                                                , encodeMutation
                                                , encodeQuery
                                                , encodeSubscription
                                                )
import           Data.Morpheus.Execution.Server.Introspect
                                                ( IntroCon
                                                , introspectObjectFields
                                                , TypeScope(..)
                                                )
import           Data.Morpheus.Execution.Server.Subscription
                                                ( GQLState
                                                , publishEvent
                                                )
import           Data.Morpheus.Parsing.Request.Parser
                                                ( parseGQL )
--import           Data.Morpheus.Schema.Schema                         (Root)
import           Data.Morpheus.Schema.SchemaAPI ( defaultTypes
                                                , hiddenRootFields
                                                , schemaAPI
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(CUSTOM) )
import           Data.Morpheus.Types.Internal.AST
                                                ( Operation(..)
                                                , DataFingerprint(..)
                                                , DataTypeContent(..)
                                                , Schema(..)
                                                , DataType(..)
                                                , MUTATION
                                                , OperationType(..)
                                                , QUERY
                                                , SUBSCRIPTION
                                                , initTypeLib
                                                , ValidValue
                                                , Name
                                                , VALIDATION_MODE(..)
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , FieldsDefinition(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..)
                                                , Resolver
                                                , GQLChannel(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , Validation
                                                , cleanEvents
                                                , ResultT(..)
                                                , unpackEvents
                                                , Failure(..)
                                                , resolveUpdates
                                                , Context(..)
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..)
                                                , GQLResponse(..)
                                                , renderResponse
                                                )
import           Data.Morpheus.Validation.Query.Validation
                                                ( validateRequest )
import           Data.Typeable                  ( Typeable )
import           Control.Monad.IO.Class         ( MonadIO() )

type EventCon event
  = (Eq (StreamChannel event), Typeable event, GQLChannel event)

type IntrospectConstraint m event query mutation subscription
  = ( IntroCon (query (Resolver QUERY event m))
    , IntroCon (mutation (Resolver MUTATION event m))
    , IntroCon (subscription (Resolver SUBSCRIPTION event m))
    )

type RootResCon m event query mutation subscription
  = ( EventCon event
    , Typeable m
    , IntrospectConstraint m event query mutation subscription
    , EncodeCon QUERY event m (query (Resolver QUERY event m))
    , EncodeCon MUTATION event m (mutation (Resolver MUTATION event m))
    , EncodeCon
        SUBSCRIPTION
        event
        m
        (subscription (Resolver SUBSCRIPTION event m))
    )

decodeNoDup :: Failure String m => L.ByteString -> m GQLRequest
decodeNoDup str = case eitherDecodeWith jsonNoDup ifromJSON str of
  Left  (path, x) -> failure $ formatError path x
  Right value     -> pure value


byteStringIO
  :: Monad m => (GQLRequest -> m GQLResponse) -> L.ByteString -> m L.ByteString
byteStringIO resolver request = case decodeNoDup request of
  Left  aesonError' -> return $ badRequestError aesonError'
  Right req         -> encode <$> resolver req

statelessResolver
  :: (Monad m, RootResCon m event query mut sub)
  => GQLRootResolver m event query mut sub
  -> GQLRequest
  -> m GQLResponse
statelessResolver root req =
  renderResponse <$> runResultT (coreResolver root req)

streamResolver
  :: forall event m query mut sub
   . (Monad m, RootResCon m event query mut sub)
  => GQLRootResolver m event query mut sub
  -> GQLRequest
  -> ResponseStream event m GQLResponse
streamResolver root req =
  ResultT $ pure . renderResponse <$> runResultT (coreResolver root req)


coreResolver
  :: forall event m query mut sub
   . (Monad m, RootResCon m event query mut sub)
  => GQLRootResolver m event query mut sub
  -> GQLRequest
  -> ResponseStream event m ValidValue
coreResolver root@GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver } request
  = validRequest >>= execOperator
 where
  validRequest
    :: Monad m => ResponseStream event m Context
  validRequest = cleanEvents $ ResultT $ pure $ do
    schema     <- fullSchema $ Identity root
    operation  <- parseGQL request >>= validateRequest schema FULL_VALIDATION
    pure $ Context {
        schema
      , operation
      , currentSelection = (
        "Root"
        , Selection {
          selectionArguments = []
          , selectionPosition = (operationPosition operation)
          , selectionAlias = Nothing
          , selectionContent = SelectionSet (operationSelection operation)
        } 
    )
  }
  ----------------------------------------------------------
  execOperator ctx@Context {schema ,operation = Operation{ operationType} } = execOperationBy operationType ctx
    where
      execOperationBy Query = encodeQuery (schemaAPI schema) queryResolver
      execOperationBy Mutation = encodeMutation mutationResolver
      execOperationBy Subscription = encodeSubscription subscriptionResolver
    
statefulResolver
  :: (EventCon event, MonadIO m)
  => GQLState m event
  -> (GQLRequest -> ResponseStream event m ValidValue)
  -> L.ByteString
  -> m L.ByteString
statefulResolver state streamApi requestText = do
  res <- runResultT (decodeNoDup requestText >>= streamApi)
  mapM_ execute (unpackEvents res)
  pure $ encode $ renderResponse res
 where
  execute (Publish events) = publishEvent state events
  execute Subscribe{}       = pure ()

fullSchema
  :: forall proxy m event query mutation subscription
   . (IntrospectConstraint m event query mutation subscription)
  => proxy (GQLRootResolver m event query mutation subscription)
  -> Validation Schema
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
 where
  querySchema = resolveUpdates
    (initTypeLib (operatorType (hiddenRootFields <> fields) "Query"))
    (defaultTypes : types)
   where
    (fields, types) = introspectObjectFields
      (Proxy @(CUSTOM (query (Resolver QUERY event m))))
      ("type for query", OutputType, Proxy @(query (Resolver QUERY event m)))
  ------------------------------
  mutationSchema lib = resolveUpdates
    (lib { mutation = maybeOperator fields "Mutation" })
    types
   where
    (fields, types) = introspectObjectFields
      (Proxy @(CUSTOM (mutation (Resolver MUTATION event m))))
      ( "type for mutation"
      , OutputType
      , Proxy @(mutation (Resolver MUTATION event m))
      )
  ------------------------------
  subscriptionSchema lib = resolveUpdates
    (lib { subscription = maybeOperator fields "Subscription" })
    types
   where
    (fields, types) = introspectObjectFields
      (Proxy @(CUSTOM (subscription (Resolver SUBSCRIPTION event m))))
      ( "type for subscription"
      , OutputType
      , Proxy @(subscription (Resolver SUBSCRIPTION event m))
      )
  maybeOperator :: FieldsDefinition -> Name -> Maybe DataType
  maybeOperator (FieldsDefinition x) | null x     = const Nothing
  maybeOperator fields = Just . operatorType fields
  -------------------------------------------------
  operatorType :: FieldsDefinition -> Name -> DataType
  operatorType fields typeName = DataType 
      { typeContent     = DataObject [] fields
        , typeName
        , typeFingerprint = DataFingerprint typeName []
        , typeMeta        = Nothing
      }