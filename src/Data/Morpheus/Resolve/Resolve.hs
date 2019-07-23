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
import           Data.Morpheus.Resolve.Encode              (Encode, EncodeCon, EncodeSubCon, encodeStreamRes,
                                                            encodeSubStreamRes, resolveBySelection, resolversBy)
import           Data.Morpheus.Resolve.Introspect          (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Schema.SchemaAPI            (hiddenRootFields, schemaAPI, schemaTypes)
import           Data.Morpheus.Server.ClientRegister       (GQLState, publishUpdates)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.Data         (DataArguments, DataFingerprint (..), DataType (..),
                                                            DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Stream       (PublishStream, ResponseEvent (..), ResponseStream,
                                                            StreamState (..), StreamT (..), closeStream, mapS)
import           Data.Morpheus.Types.Internal.Validation   (SchemaValidation)
import           Data.Morpheus.Types.Internal.Value        (Value (..))
import           Data.Morpheus.Types.IO                    (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Resolver              (GQLRootResolver (..))
import           Data.Morpheus.Validation.Validation       (validateRequest)

type EventCon event = Eq event

type IntroCon a = (Generic a, ObjectRep (Rep a) DataArguments)

type RootResCon m event query mutation subscription
   = ( Monad m
     , EventCon event
      -- Introspection
     , IntroCon query
     , IntroCon mutation
     , IntroCon subscription
     -- Resolving
     , EncodeCon m query
     , EncodeCon (PublishStream m event) mutation
     , EncodeSubCon m event subscription)

byteStringIO :: Monad m => (GQLRequest -> m GQLResponse) -> ByteString -> m ByteString
byteStringIO resolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolver req

statelessResolver :: RootResCon m s a b c => GQLRootResolver m s a b c -> GQLRequest -> m GQLResponse
statelessResolver root request = snd <$> closeStream (streamResolver root request)

streamResolver ::
     RootResCon m s a b subscription'
  => GQLRootResolver m s a b subscription'
  -> GQLRequest
  -> ResponseStream m s GQLResponse
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
    execOperator (schema, Query Operator' {operatorSelection}) =
      StreamT $ StreamState [] <$> encodeQuery schema queryResolver operatorSelection
    execOperator (_, Mutation Operator' {operatorSelection}) =
      mapS Publish (encodeStreamRes mutationResolver operatorSelection)
    execOperator (_, Subscription Operator' {operatorSelection}) =
      StreamT $ do
        (channels, result) <- closeStream (encodeSubStreamRes subscriptionResolver operatorSelection)
        pure $
          case result of
            Left gqlError -> StreamState [] (Left gqlError)
            Right subResolver -> StreamState [Subscribe (concat channels, handleRes)] (Right Null)
              where handleRes event = renderResponse <$> runExceptT (subResolver event)

encodeQuery :: (Monad m, EncodeCon m a) => DataTypeLib -> Encode m a
encodeQuery types rootResolver sel =
  runExceptT (fmap resolversBy rootResolver >>= resolveBySelection sel . (++) (resolversBy $ schemaAPI types))

statefulResolver ::
     EventCon s => GQLState IO s -> (ByteString -> ResponseStream IO s ByteString) -> ByteString -> IO ByteString
statefulResolver state streamApi request = do
  (actions, value) <- closeStream (streamApi request)
  mapM_ execute actions
  pure value
  where
    execute (Publish updates) = publishUpdates state updates
    execute Subscribe {}      = pure ()

fullSchema ::
     forall m s query mutation subscription. (IntroCon query, IntroCon mutation, IntroCon subscription)
  => GQLRootResolver m s query mutation subscription
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
      (typeName, DataType {typeData, typeName, typeFingerprint = SystemFingerprint typeName, typeDescription = ""})
