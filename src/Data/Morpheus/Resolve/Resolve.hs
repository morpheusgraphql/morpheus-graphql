{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Resolve
  ( resolveStateless
  , resolveByteString
  , resolveStreamByteString
  , resolveStream
  , packStream
  , RootResCon
  ) where

import           Data.Functor                               (($>))

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (eitherDecode, encode)
import           Data.ByteString.Lazy.Char8                 (ByteString)
import           Data.Morpheus.Error.Utils                  (badRequestError, renderErrors)
import           Data.Morpheus.Parser.Parser                (parseGQL)
import           Data.Morpheus.Resolve.Encode               (ObjectFieldResolvers (..), resolveBySelection, resolversBy)
import           Data.Morpheus.Resolve.Generics.TypeRep     (ObjectRep (..), resolveTypes)
import           Data.Morpheus.Schema.SchemaAPI             (hiddenRootFields, schemaAPI, schemaTypes)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataArguments, DataFingerprint (..), DataType (..),
                                                             DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Internal.WebSocket     (OutputAction (..), WSSubscription (..))
import           Data.Morpheus.Types.IO                     (GQLRequest (..), GQLResponse (..))
import           Data.Morpheus.Types.Resolver               (GQLRootResolver (..), StreamT (..), SubT, unpackStream2)
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.Proxy
import           Data.Typeable                              (Typeable)
import           GHC.Generics

type IntroCon a = (Generic a, ObjectRep (Rep a) DataArguments)

type EncodeCon m a = (Generic a, Typeable a, ObjectFieldResolvers (Rep a) m)

type RootResCon m s a b c
   = ( Typeable s
     , Monad m
     , IntroCon a
     , IntroCon b
     , IntroCon c
     , EncodeCon m a
     , EncodeCon (StreamT m s) b
     , EncodeCon (StreamT m (SubT m s)) c)

resolveByteString :: RootResCon m s a b c => GQLRootResolver m s a b c -> ByteString -> m ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolveStateless rootResolver req

resolveStreamByteString ::
     RootResCon m s a b c => GQLRootResolver m s a b c -> ByteString -> m (OutputAction m s ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoAction $ badRequestError aesonError'
    Right req        -> fmap encode <$> resolveStream rootResolver req

resolveStateless :: RootResCon m s a b c => GQLRootResolver m s a b c -> GQLRequest -> m GQLResponse
resolveStateless root request = resolveStream root request >>= executeActions
  where
    executeActions PublishMutation {mutationResponse} = return mutationResponse
    executeActions InitSubscription {}                = pure $ Errors [] -- TODO
    executeActions (NoAction response)                = return response

resolveStream ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> GQLRequest -> m (OutputAction m s GQLResponse)
resolveStream root@GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case fullSchema root of
    Left error' -> return $ NoAction $ Errors $ renderErrors error'
    Right validSchema -> do
      value <- runExceptT $ _resolve validSchema
      case value of
        Left x       -> return $ NoAction $ Errors $ renderErrors x
        Right value' -> return $ fmap Data value'
  where
    _resolve gqlSchema = (ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)) >>= resolveOperator
      where
        resolveOperator (Query Operator' {operatorSelection}) =
          NoAction <$> encodeQuery gqlSchema queryResolver operatorSelection
        resolveOperator (Mutation Operator' {operatorSelection}) = do
          (mutationChannels, mutationResponse) <- unpackStream2 (encodeStreamRes mutationResolver operatorSelection)
          return PublishMutation {mutationChannels, mutationResponse}
        resolveOperator (Subscription Operator' {operatorSelection}) = do
          (channels, _) <- unpackStream2 (encodeStreamRes subscriptionResolver operatorSelection)
          let (subscriptionChannels, resolvers) = unzip channels
          return $ InitSubscription $ WSSubscription {subscriptionChannels, subscriptionRes = subRes $ head resolvers}
          where
            subRes :: Monad m => (s -> ResolveT m Value) -> s -> m GQLResponse
            subRes resolver arg = do
              value <- runExceptT $ resolver arg
              case value of
                Left x  -> pure $ Errors $ renderErrors x
                Right v -> return $ Data v

type Encode m a = ResolveT m a -> SelectionSet -> ResolveT m Value

encodeQuery :: (Monad m, EncodeCon m a) => DataTypeLib -> Encode m a
encodeQuery types rootResolver sel =
  fmap resolversBy rootResolver >>= resolveBySelection sel . (++) (resolversBy $ schemaAPI types)

encodeStreamRes :: (Monad m, EncodeCon m a) => Encode m a
encodeStreamRes rootResolver sel = rootResolver >>= resolveBySelection sel . resolversBy

packStream ::
     (Show s, Eq s) => GQLState IO s -> (ByteString -> IO (OutputAction IO s ByteString)) -> ByteString -> IO ByteString
packStream state streamAPI request = streamAPI request >>= executeActions
  where
    executeActions PublishMutation {mutationChannels, mutationResponse} =
      publishUpdates mutationChannels state $> mutationResponse
    executeActions InitSubscription {} = pure "subscriptions are only allowed in websocket"
    executeActions (NoAction response) = return response

fullSchema ::
     forall m s a b c. (IntroCon a, IntroCon b, IntroCon c)
  => GQLRootResolver m s a b c
  -> SchemaValidation DataTypeLib
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
  where
    querySchema = resolveTypes (initTypeLib (operatorType (hiddenRootFields ++ fields) "Query")) (schemaTypes : types)
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))
    mutationSchema lib = resolveTypes (lib {mutation = maybeOperator fields "Mutation"}) types
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep b))
    subscriptionSchema lib = resolveTypes (lib {subscription = maybeOperator fields "Subscription"}) types
      where
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep c))
     -- maybeOperator :: [a] -> Text -> Maybe (Text, DataType [a])
    maybeOperator []     = const Nothing
    maybeOperator fields = Just . operatorType fields
    -- operatorType :: [a] -> Text -> (Text, DataType [a])
    operatorType typeData typeName =
      (typeName, DataType {typeData, typeName, typeFingerprint = SystemFingerprint typeName, typeDescription = ""})
