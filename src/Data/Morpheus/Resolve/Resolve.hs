{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

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
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.WebSocket     (OutputAction (..), WSSubscription (..))
import           Data.Morpheus.Types.IO                     (GQLRequest (..), GQLResponse (..))

import           Data.Morpheus.Resolve.Encode               (ObjectFieldResolvers (..), resolveBySelection, resolversBy)
import           Data.Morpheus.Resolve.Generics.TypeRep     (ObjectRep (..), TypeUpdater, resolveTypes)
import           Data.Morpheus.Schema.SchemaAPI             (hiddenRootFields, schemaAPI, schemaTypes)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataArguments, DataFingerprint (..), DataType (..),
                                                             DataTypeLib (..), initTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (GQLRootResolver (..), MutStream, StreamT (..), SubStream,
                                                             SubT, unpackStream2)
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.Proxy
import           Data.Text                                  (Text)
import           Data.Typeable                              (Typeable)
import           GHC.Generics

resolveByteString ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> ByteString -> m ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolve rootResolver req

resolveStreamByteString ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> ByteString -> m (OutputAction m s ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoAction $ badRequestError aesonError'
    Right req        -> fmap encode <$> resolveStream rootResolver req

resolve ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> GQLRequest -> m GQLResponse
resolve GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case fullSchema queryResolver mutationResolver subscriptionResolver of
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
                  snd <$> unpackStream2 (effectEncode mutationResolver (operatorSelection operator'))
                Subscription operator' ->
                  snd <$> unpackStream2 (encodeSub subscriptionResolver (operatorSelection operator'))

resolveStream ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> GQLRequest -> m (OutputAction m s GQLResponse)
resolveStream GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case fullSchema queryResolver mutationResolver subscriptionResolver of
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
            unpackStream2 $ effectEncode mutationResolver $ operatorSelection operator'
          return PublishMutation {mutationChannels, mutationResponse}
        resolveOperator (Subscription operator') = do
          (channels, _) <- unpackStream2 $ effectEncode subscriptionResolver $ operatorSelection operator'
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

type RootResCon m s a b c = (Typeable s, OperatorCon m a, OperatorStreamCon m s b, OperatorStreamCon m (SubT m s) c)

type OperatorCon m a = (IntroCon a, EncodeCon m a)

type OperatorStreamCon m s a = (IntroCon a, EncodeCon (StreamT m s) a)

type Encode m a = ResolveT m a -> SelectionSet -> ResolveT m Value

type EncodeCon m a = (Generic a, ObjectFieldResolvers (Rep a) m)

type BaseEncode m a
   = EncodeCon m a =>
       DataTypeLib -> Encode m a

type StreamEncode m s a
   = EncodeCon (StreamT m s) a =>
       Encode (StreamT m s) a

encodeQuery :: Monad m => BaseEncode m a
encodeQuery types rootResolver sel =
  fmap resolversBy rootResolver >>= resolveBySelection sel . (++) (resolversBy $ schemaAPI types)

effectEncode :: Monad m => StreamEncode m s a
effectEncode rootResolver sel = rootResolver >>= resolveBySelection sel . resolversBy

encodeSub :: Monad m => StreamEncode m (s, s -> ResolveT m Value) a
encodeSub rootResolver sel = rootResolver >>= resolveBySelection sel . resolversBy

type IntroCon a = (Generic a, ObjectRep (Rep a) DataArguments, Typeable a)

fullSchema ::
     forall m s a b c. (IntroCon a, IntroCon b, IntroCon c)
  => ResolveT m a
  -> ResolveT (MutStream m s) b
  -> ResolveT (SubStream m s) c
  -> SchemaValidation DataTypeLib
fullSchema queryRes mutationRes subscriptionRes =
  querySchema queryRes >>= mutationSchema mutationRes >>= subscriptionSchema subscriptionRes
  where
    querySchema _ = resolveTypes queryType (schemaTypes : types)
      where
        queryType = initTypeLib (operatorType (hiddenRootFields ++ fields) "Query")
        (fields, types) = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

mutationSchema ::
     forall a m. IntroCon a
  => m a
  -> TypeUpdater
mutationSchema _ initialType = resolveTypes mutationType types'
  where
    mutationType = initialType {mutation = maybeOperator fields "Mutation"}
    (fields, types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

subscriptionSchema ::
     forall a m. IntroCon a
  => m a
  -> TypeUpdater
subscriptionSchema _ initialType = resolveTypes mutationType types'
  where
    mutationType = initialType {subscription = maybeOperator fields "Subscription"}
    (fields, types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

maybeOperator :: [a] -> Text -> Maybe (Text, DataType [a])
maybeOperator []     = const Nothing
maybeOperator fields = Just . operatorType fields

operatorType :: [a] -> Text -> (Text, DataType [a])
operatorType typeData typeName =
  (typeName, DataType {typeData, typeName, typeFingerprint = SystemFingerprint typeName, typeDescription = ""})
