{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Resolve.Resolve
  ( resolve
  , resolveByteString
  , resolveStreamByteString
  , resolveStream
  , packStream
  ) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Error.Utils (badRequestError, renderErrors)
import Data.Morpheus.Parser.Parser (parseGQL)
import Data.Morpheus.Resolve.Operator (RootResCon, effectEncode, encodeQuery, encodeSub, fullSchema)
import Data.Morpheus.Server.ClientRegister (GQLState, publishUpdates)
import Data.Morpheus.Types.IO (GQLRequest(..), GQLResponse(..))
import Data.Morpheus.Types.Internal.AST.Operator (Operator(..), Operator'(..))
import Data.Morpheus.Types.Internal.Validation (ResolveT)
import Data.Morpheus.Types.Internal.Value (Value)
import Data.Morpheus.Types.Internal.WebSocket (OutputAction(..), WSSubscription(..))

import Data.Morpheus.Types.Resolver (GQLRootResolver(..), unpackStream2)
import Data.Morpheus.Validation.Validation (validateRequest)

resolveByteString ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> ByteString -> m ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req -> encode <$> resolve rootResolver req

resolveStreamByteString ::
     Monad m
  => RootResCon m s a b c =>
       GQLRootResolver m s a b c -> ByteString -> m (OutputAction m s ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoAction $ badRequestError aesonError'
    Right req -> fmap encode <$> resolveStream rootResolver req

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
        Left x -> return $ Errors $ renderErrors x
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
        Left x -> return $ NoAction $ Errors $ renderErrors x
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
                Left x -> pure $ Errors $ renderErrors x
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