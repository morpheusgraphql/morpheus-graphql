{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Morpheus.Resolve.Resolve
  ( resolve
  , resolveByteString
  , resolveStreamByteString
  , resolveStream
  , packStream
  , RootResCon
  ) where

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

type EncodeCon m a = (Generic a, ObjectFieldResolvers (Rep a) m)

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
    Right req        -> encode <$> resolve rootResolver req

resolveStreamByteString ::
     RootResCon m s a b c => GQLRootResolver m s a b c -> ByteString -> m (OutputAction m s ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoAction $ badRequestError aesonError'
    Right req        -> fmap encode <$> resolveStream rootResolver req

resolve :: RootResCon m s a b c => GQLRootResolver m s a b c -> GQLRequest -> m GQLResponse
resolve root@GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case fullSchema root of
    Left error' -> return $ Errors $ renderErrors error'
    Right validSchema -> do
      value <- runExceptT $ _resolve validSchema
      case value of
        Left x  -> return $ Errors $ renderErrors x
        Right x -> return $ Data x
      where _resolve gqlSchema = do
              rootGQL <- ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)
              case rootGQL of
                Query operator' -> encodeQuery gqlSchema queryResolver $ operatorSelection operator'
                Mutation operator' ->
                  snd <$> unpackStream2 (encodeStreamRes mutationResolver (operatorSelection operator'))
                Subscription operator' ->
                  snd <$> unpackStream2 (encodeStreamRes subscriptionResolver (operatorSelection operator'))

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
        resolveOperator (Query operator') = do
          value <- encodeQuery gqlSchema queryResolver $ operatorSelection operator'
          return (NoAction value)
        resolveOperator (Mutation operator') = do
          (mutationChannels, mutationResponse) <-
            unpackStream2 $ encodeStreamRes mutationResolver $ operatorSelection operator'
          return PublishMutation {mutationChannels, mutationResponse}
        resolveOperator (Subscription operator') = do
          (channels, _) <- unpackStream2 $ encodeStreamRes subscriptionResolver $ operatorSelection operator'
          let (subscriptionChannels, resolvers) = unzip channels
          return $ InitSubscription $ WSSubscription {subscriptionChannels, subscriptionRes = subRes $ head resolvers}
          where
            subRes :: Monad m => (s -> ResolveT m Value) -> s -> m GQLResponse
            subRes resolver arg = do
              value <- runExceptT $ resolver arg
              case value of
                Left x  -> pure $ Errors $ renderErrors x
                Right v -> return $ Data v

packStream ::
     (Show s, Eq s) => GQLState IO s -> (ByteString -> IO (OutputAction IO s ByteString)) -> ByteString -> IO ByteString
packStream state streamAPI request = do
  value <- streamAPI request
  case value of
    PublishMutation {mutationChannels, mutationResponse} -> do
      publishUpdates mutationChannels state
      return mutationResponse
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoAction res' -> return res'

type Encode m a = ResolveT m a -> SelectionSet -> ResolveT m Value

encodeQuery :: (Monad m, EncodeCon m a) => DataTypeLib -> Encode m a
encodeQuery types rootResolver sel =
  fmap resolversBy rootResolver >>= resolveBySelection sel . (++) (resolversBy $ schemaAPI types)

encodeStreamRes :: (Monad m, EncodeCon m a) => Encode m a
encodeStreamRes rootResolver sel = rootResolver >>= resolveBySelection sel . resolversBy

type IntroCon a = (Generic a, ObjectRep (Rep a) DataArguments, Typeable a)

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
