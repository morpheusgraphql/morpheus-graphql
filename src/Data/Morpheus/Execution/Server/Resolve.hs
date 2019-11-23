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
                                                , ObjectFields(..)
                                                )
import           Data.Morpheus.Execution.Subscription.ClientRegister
                                                ( GQLState
                                                , publishUpdates
                                                )
import           Data.Morpheus.Parsing.Request.Parser
                                                ( parseGQL )
--import           Data.Morpheus.Schema.Schema                         (Root)
import           Data.Morpheus.Schema.SchemaAPI ( defaultTypes
                                                , hiddenRootFields
                                                , schemaAPI
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(CUSTOM) )
import           Data.Morpheus.Types.Internal.AST.Operation
                                                ( Operation(..)
                                                , ValidOperation
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataFingerprint(..)
                                                , DataTyCon(..)
                                                , DataTypeLib(..)
                                                , MUTATION
                                                , OperationType(..)
                                                , QUERY
                                                , SUBSCRIPTION
                                                , initTypeLib
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..)
                                                , Resolver(..)
                                                , ResponseT
                                                , toResponseRes
                                                , GQLChannel(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , Validation
                                                , cleanEvents
                                                , ResultT(..)
                                                , unpackEvents
                                                , Failure(..)
                                                , resolveUpdates
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..)
                                                , GQLResponse(..)
                                                , renderResponse
                                                )
import           Data.Morpheus.Validation.Internal.Utils
                                                ( VALIDATION_MODE(..) )
import           Data.Morpheus.Validation.Query.Validation
                                                ( validateRequest )
import           Data.Typeable                  ( Typeable )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )


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
    -- , OBJ_RES m (Root (Resolver m)) Value
     -- Resolving
    , EncodeCon QUERY event m (query (Resolver QUERY event m))
    , EncodeCon MUTATION event m (mutation (Resolver MUTATION event m))
    , EncodeCon
        SUBSCRIPTION
        event
        m
        (subscription (Resolver SUBSCRIPTION event m))
    )

--decodeNoDup :: L.ByteString -> Either String GQLRequest
--decodeNoDup str = case eitherDecodeWith jsonNoDup ifromJSON str of
--  Left  (path, x) -> Left $ formatError path x
--  Right value     -> Right value

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
  -> ResponseStream event m Value
coreResolver root@GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver } request
  = validRequest >>= execOperator
 where
  validRequest :: Monad m => ResponseT event m (DataTypeLib, ValidOperation)
  validRequest = cleanEvents $ ResultT $ pure $ do
    schema <- fullSchema $ Identity root
    query  <- parseGQL request >>= validateRequest schema FULL_VALIDATION
    pure (schema, query)
  ----------------------------------------------------------
  execOperator (schema, operation@Operation { operationType = Query }) =
    toResponseRes (encodeQuery (schemaAPI schema) queryResolver operation)
  execOperator (_, operation@Operation { operationType = Mutation }) =
    toResponseRes (encodeMutation mutationResolver operation)
  execOperator (_, operation@Operation { operationType = Subscription }) =
    response
   where
    response =
      toResponseRes (encodeSubscription subscriptionResolver operation)

statefulResolver
  :: EventCon event
  => GQLState IO event
  -> (GQLRequest -> ResponseStream event IO Value)
  -> L.ByteString
  -> IO L.ByteString
statefulResolver state streamApi requestText = do
  res <- runResultT (decodeNoDup requestText >>= streamApi)
  mapM_ execute (unpackEvents res)
  pure $ encode $ renderResponse res
 where
  execute (Publish updates) = publishUpdates state updates
  execute Subscribe{}       = pure ()


fullSchema
  :: forall proxy m event query mutation subscription
   . (IntrospectConstraint m event query mutation subscription)
  => proxy (GQLRootResolver m event query mutation subscription)
  -> Validation DataTypeLib
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
 where
  querySchema = resolveUpdates
    (initTypeLib (operatorType (hiddenRootFields ++ fields) "Query"))
    (defaultTypes : types)
   where
    (fields, types) = objectFields
      (Proxy @(CUSTOM (query (Resolver QUERY event m))))
      (Proxy @(query (Resolver QUERY event m)))
  ------------------------------
  mutationSchema lib = resolveUpdates
    (lib { mutation = maybeOperator fields "Mutation" })
    types
   where
    (fields, types) = objectFields
      (Proxy @(CUSTOM (mutation (Resolver MUTATION event m))))
      (Proxy @(mutation (Resolver MUTATION event m)))
  ------------------------------
  subscriptionSchema lib = resolveUpdates
    (lib { subscription = maybeOperator fields "Subscription" })
    types
   where
    (fields, types) = objectFields
      (Proxy @(CUSTOM (subscription (Resolver SUBSCRIPTION event m))))
      (Proxy @(subscription (Resolver SUBSCRIPTION event m)))
   -- maybeOperator :: [a] -> Text -> Maybe (Text, DataTyCon[a])
  maybeOperator []     = const Nothing
  maybeOperator fields = Just . operatorType fields
  -- operatorType :: [a] -> Text -> (Text, DataTyCon[a])
  operatorType typeData typeName =
    ( typeName
    , DataTyCon { typeData
                , typeName
                , typeFingerprint = SystemFingerprint typeName
                , typeMeta        = Nothing
                }
    )
