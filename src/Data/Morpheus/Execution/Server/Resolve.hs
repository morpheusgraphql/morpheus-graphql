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
  ) where

import           Control.Monad.Except                                (liftEither)
import           Control.Monad.Trans.Except                          (ExceptT (..), runExceptT)
import           Data.Aeson                                          (encode)
import           Data.Aeson.Internal                                 (formatError, ifromJSON)
import           Data.Aeson.Parser                                   (eitherDecodeWith, jsonNoDup)
import qualified Data.ByteString.Lazy.Char8                          as L
import           Data.Functor.Identity                               (Identity (..))
import           Data.Proxy                                          (Proxy (..))

-- MORPHEUS
import           Data.Morpheus.Error.Utils                           (badRequestError, renderErrors)
import           Data.Morpheus.Execution.Internal.GraphScanner       (resolveUpdates)
import           Data.Morpheus.Execution.Server.Encode               (EncodeCon, encodeOperation, encodeQuery)
import           Data.Morpheus.Execution.Server.Introspect           (IntroCon, ObjectFields (..))
import           Data.Morpheus.Execution.Subscription.ClientRegister (GQLState, publishUpdates)
import           Data.Morpheus.Parsing.Request.Parser                (parseGQL)
--import           Data.Morpheus.Schema.Schema                         (Root)
import           Data.Morpheus.Schema.SchemaAPI                      (defaultTypes, hiddenRootFields, schemaAPI)
import           Data.Morpheus.Types.GQLType                         (GQLType (CUSTOM))
import           Data.Morpheus.Types.Internal.AST.Operation          (Operation (..), ValidOperation)
import           Data.Morpheus.Types.Internal.Data                   (DataFingerprint (..), DataTyCon (..),
                                                                      DataTypeLib (..), MUTATION, OperationKind (..),
                                                                      QUERY, SUBSCRIPTION, initTypeLib)
import           Data.Morpheus.Types.Internal.Resolver               (GADTResolver (..), GQLRootResolver (..),
                                                                      MutResolver, ResponseT, SubResolver,
                                                                      toResponseRes)
import           Data.Morpheus.Types.Internal.Stream                 (Event (..), GQLChannel (..), ResponseEvent (..),
                                                                      ResponseStream, StreamState (..), StreamT (..),
                                                                      closeStream, mapS)
import           Data.Morpheus.Types.Internal.Validation             (Validation)
import           Data.Morpheus.Types.Internal.Value                  (Value (..))
import           Data.Morpheus.Types.IO                              (GQLRequest (..), GQLResponse (..), renderResponse)
import           Data.Morpheus.Validation.Internal.Utils             (VALIDATION_MODE (..))
import           Data.Morpheus.Validation.Query.Validation           (validateRequest)
import           Data.Typeable                                       (Typeable)


type EventCon event = (Eq (StreamChannel event), GQLChannel event)

type IntrospectConstraint  m event query mutation subscription = (
                                  IntroCon (query (GADTResolver 'Query m event))
                                 , IntroCon (mutation (MutResolver m event))
                                 , IntroCon (subscription (SubResolver m event)))

type RootResCon m event query mutation subscription
   = ( EventCon event
     , Typeable m
     , IntrospectConstraint m event query mutation subscription
    -- , OBJ_RES m (Root (Resolver m)) Value
     -- Resolving
     , EncodeCon QUERY m event (query (GADTResolver QUERY m event))
     , EncodeCon MUTATION m event (mutation (GADTResolver MUTATION m event))
     , EncodeCon SUBSCRIPTION m event (subscription (GADTResolver SUBSCRIPTION m event)))

decodeNoDup :: L.ByteString -> Either String GQLRequest
decodeNoDup str =
  case eitherDecodeWith jsonNoDup ifromJSON str of
    Left (path, x) -> Left $ formatError path x
    Right value    -> Right value

byteStringIO :: Monad m => (GQLRequest -> m GQLResponse) -> L.ByteString -> m L.ByteString
byteStringIO resolver request =
  case decodeNoDup request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolver req

statelessResolver ::
     (Monad m, RootResCon m event query mut sub)
  => GQLRootResolver m event  query mut sub
  -> GQLRequest
  -> m GQLResponse
statelessResolver root = fmap snd . closeStream . streamResolver root

streamResolver ::
     (Monad m, RootResCon m event  query mut sub)
  => GQLRootResolver m event query mut sub
  -> GQLRequest
  -> ResponseStream m event GQLResponse
streamResolver root@GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  renderResponse <$> runExceptT (validRequest >>= execOperator)
  ------------------------------------------------------------
  where

    ---------------------------------------------------------
    validRequest :: Monad m => ResponseT m event (DataTypeLib, ValidOperation)
    validRequest =
      liftEither $ do
        schema <- fullSchema $ Identity root
        query <- parseGQL request >>= validateRequest schema FULL_VALIDATION
        Right (schema, query)
    ----------------------------------------------------------
    execOperator (schema, operation@Operation {operationKind = Query}) =
        --TODO:  -- do  $ schemaRes <- schemaAPI schema
        toResponseRes (encodeQuery ({- TODO: schemaRes -}) queryResolver operation)
    execOperator (_, operation@Operation {operationKind = Mutation}) = toResponseRes (encodeOperation mutationResolver operation)
    execOperator (_, operation@Operation {operationKind = Subscription}) = toResponseRes (encodeOperation subscriptionResolver operation)
     -- ExceptT $ StreamT $ handleActions <$> closeStream (encodeOperation subscriptionResolver operation)
     -- where
     --   handleActions (_, Left gqlError) = StreamState [] (Left gqlError)
 --       handleActions (channels, Right subResolver) =
 --         StreamState [Subscribe $ Event (concat channels) handleRes] (Right Null)
 --         where
 --           handleRes event = renderResponse <$> runExceptT (subResolver event)

statefulResolver ::
     EventCon s
  => GQLState IO s
  -> (L.ByteString -> ResponseStream IO s  L.ByteString)
  -> L.ByteString
  -> IO L.ByteString
statefulResolver state streamApi request = do
  (actions, value) <- closeStream (streamApi request)
  mapM_ execute actions
  pure value
  where
    execute (Publish updates) = publishUpdates state updates
    execute Subscribe {}      = pure ()

fullSchema ::
     forall proxy m event query mutation subscription. (IntrospectConstraint m event query mutation subscription)
  => proxy (GQLRootResolver m event query mutation subscription)
  -> Validation DataTypeLib
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
  where
    querySchema =
      resolveUpdates (initTypeLib (operatorType (hiddenRootFields ++ fields) "Query")) (defaultTypes : types)
      where
        (fields, types) = objectFields (Proxy @(CUSTOM (query (GADTResolver 'Query m event)))) (Proxy @(query (GADTResolver 'Query m event)))
    ------------------------------
    mutationSchema lib = resolveUpdates (lib {mutation = maybeOperator fields "Mutation"}) types
      where
        (fields, types) = objectFields (Proxy @(CUSTOM (mutation (MutResolver m event)))) (Proxy @(mutation (MutResolver m event)))
    ------------------------------
    subscriptionSchema lib = resolveUpdates (lib {subscription = maybeOperator fields "Subscription"}) types
      where
        (fields, types) = objectFields (Proxy @(CUSTOM (subscription (SubResolver m event)))) (Proxy @(subscription (SubResolver  m event)))
     -- maybeOperator :: [a] -> Text -> Maybe (Text, DataTyCon[a])
    maybeOperator []     = const Nothing
    maybeOperator fields = Just . operatorType fields
    -- operatorType :: [a] -> Text -> (Text, DataTyCon[a])
    operatorType typeData typeName =
      ( typeName
      , DataTyCon {typeData, typeName, typeFingerprint = SystemFingerprint typeName, typeDescription = Nothing})
