{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Resolve
  ( statelessResolver
  , byteStringIO
  , streamResolver
  , statefulResolver
  , RootResCon
  , fullSchema
  ) where

import           Control.Monad.Trans.Except                (ExceptT (..), runExceptT)
import           Data.Aeson                                (eitherDecode, encode)
import           Data.ByteString.Lazy.Char8                (ByteString)
import           Data.Proxy
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Utils                 (badRequestError, renderErrors)
import           Data.Morpheus.Parser.Parser               (parseGQL)
import           Data.Morpheus.Resolve.Encode              (EncodeCon, EncodeMutCon, EncodeSubCon, encodeMut,
                                                            encodeQuery, encodeSub)
import           Data.Morpheus.Resolve.Introspect          (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Schema.SchemaAPI            (hiddenRootFields, schemaAPI, schemaTypes)
import           Data.Morpheus.Server.ClientRegister       (GQLState, publishUpdates)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..))
import           Data.Morpheus.Types.Internal.Data         (DataArguments, DataFingerprint (..), DataType (..),
                                                            DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Stream       (Event (..), ResponseEvent (..), ResponseStream,
                                                            StreamState (..), StreamT (..), closeStream, mapS)
import           Data.Morpheus.Types.Internal.Validation   (SchemaValidation)
import           Data.Morpheus.Types.Internal.Value        (Value (..))
import           Data.Morpheus.Types.IO                    (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Resolver              (GQLRootResolver (..))
import           Data.Morpheus.Validation.Validation       (validateRequest)

type EventCon event = Eq event

type IntroCon a = (Generic a, ObjectRep (Rep a) DataArguments)

type RootResCon m event cont query mutation subscription
   = ( EventCon event
      -- Introspection
     , IntroCon query
     , IntroCon mutation
     , IntroCon subscription
     -- Resolving
     , EncodeCon m query Value
     , EncodeMutCon m event cont mutation
     , EncodeSubCon m event cont subscription)

byteStringIO :: Monad m => (GQLRequest -> m GQLResponse) -> ByteString -> m ByteString
byteStringIO resolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolver req

statelessResolver ::
     (Monad m, RootResCon m s cont query mut sub)
  => GQLRootResolver m s cont query mut sub
  -> GQLRequest
  -> m GQLResponse
statelessResolver root = fmap snd . closeStream . streamResolver root

streamResolver ::
     (Monad m, RootResCon m s cont query mut sub)
  => GQLRootResolver m s cont query mut sub
  -> GQLRequest
  -> ResponseStream m s cont GQLResponse
streamResolver root@GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  renderResponse <$> runExceptT (ExceptT (pure validRequest) >>= ExceptT . execOperator)
  where
    renderResponse (Left errors) = Errors $ renderErrors errors
    renderResponse (Right value) = Data value
    ---------------------------------------------------------
    validRequest = do
      schema <- fullSchema root
      query <- parseGQL request >>= validateRequest schema
      return (schema, query)
    ----------------------------------------------------------
    execOperator (schema, Query operator) =
      StreamT $ StreamState [] <$> encodeQuery (schemaAPI schema) queryResolver operator
    execOperator (_, Mutation operator) = mapS Publish (encodeMut mutationResolver operator)
    execOperator (_, Subscription operator) =
      StreamT $ handleActions <$> closeStream (encodeSub subscriptionResolver operator)
      where
        handleActions (_, Left gqlError) = StreamState [] (Left gqlError)
        handleActions (channels, Right subResolver) =
          StreamState [Subscribe $ Event (concat channels) handleRes] (Right Null)
          where
            handleRes event = renderResponse <$> runExceptT (subResolver event)

statefulResolver ::
     EventCon s
  => GQLState IO s cont
  -> (ByteString -> ResponseStream IO s cont ByteString)
  -> ByteString
  -> IO ByteString
statefulResolver state streamApi request = do
  (actions, value) <- closeStream (streamApi request)
  mapM_ execute actions
  pure value
  where
    execute (Publish updates) = publishUpdates state updates
    execute Subscribe {}      = pure ()

fullSchema ::
     forall m s cont query mutation subscription. (IntroCon query, IntroCon mutation, IntroCon subscription)
  => GQLRootResolver m s cont query mutation subscription
  -> SchemaValidation DataTypeLib
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
  where
    querySchema = resolveTypes (initTypeLib (operatorType (hiddenRootFields ++ fields) "Query")) (schemaTypes : types)
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep query))
    ------------------------------
    mutationSchema lib = resolveTypes (lib {mutation = maybeOperator fields "Mutation"}) types
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep mutation))
    ------------------------------
    subscriptionSchema lib = resolveTypes (lib {subscription = maybeOperator fields "Subscription"}) types
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep subscription))
     -- maybeOperator :: [a] -> Text -> Maybe (Text, DataType [a])
    maybeOperator []     = const Nothing
    maybeOperator fields = Just . operatorType fields
    -- operatorType :: [a] -> Text -> (Text, DataType [a])
    operatorType typeData typeName =
      ( typeName
      , DataType
          { typeData
          , typeVisibility = True
          , typeName
          , typeFingerprint = SystemFingerprint typeName
          , typeDescription = ""
          })
