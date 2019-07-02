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
import Data.Morpheus.Resolve.Operator (RootResCon, effectEncode, encodeQuery, fullSchema)
import Data.Morpheus.Server.ClientRegister (GQLState, publishUpdates)
import Data.Morpheus.Types.IO (GQLRequest(..), GQLResponse(..))
import Data.Morpheus.Types.Internal.AST.Operator (Operator(..), Operator'(..))
import Data.Morpheus.Types.Internal.WebSocket (OutputAction(..))
import Data.Morpheus.Types.Resolver (GQLRootResolver(..), unpackStream, unpackStream2)
import Data.Morpheus.Validation.Validation (validateRequest)

resolveByteString ::
     Monad m
  => RootResCon m a b c =>
       GQLRootResolver m a b c -> ByteString -> m ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req -> encode <$> resolve rootResolver req

resolveStreamByteString ::
     Monad m
  => RootResCon m a b c =>
       GQLRootResolver m a b c -> ByteString -> m (OutputAction m ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoAction $ badRequestError aesonError'
    Right req -> fmap encode <$> resolveStream rootResolver req

resolve ::
     Monad m
  => RootResCon m a b c =>
       GQLRootResolver m a b c -> GQLRequest -> m GQLResponse
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
                  snd <$> unpackStream2 (effectEncode subscriptionResolver (operatorSelection operator'))

resolveStream ::
     Monad m
  => RootResCon m a b c =>
       GQLRootResolver m a b c -> GQLRequest -> m (OutputAction m GQLResponse)
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
          (channels, response) <- unpackStream2 $ effectEncode mutationResolver $ operatorSelection operator'
          return
            PublishMutation {mutationChannels = channels, mutationResponse = response, currentSubscriptionStateResolver}
          where
            currentSubscriptionStateResolver selection' = do
              value <- unpackStream (effectEncode subscriptionResolver selection')
              case value of
                Left x -> pure $ Errors $ renderErrors x
                Right (_, x') -> return $ Data x'
        resolveOperator (Subscription operator') = do
          (subscriptionChannels, _) <- unpackStream2 $ effectEncode subscriptionResolver $ operatorSelection operator'
          return InitSubscription {subscriptionChannels, subscriptionQuery = operatorSelection operator'}

packStream :: GQLState -> (ByteString -> IO (OutputAction IO ByteString)) -> ByteString -> IO ByteString
packStream state streamAPI request = do
  value <- streamAPI request
  case value of
    PublishMutation {mutationChannels, mutationResponse, currentSubscriptionStateResolver} -> do
      publishUpdates mutationChannels currentSubscriptionStateResolver state
      return mutationResponse
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoAction res' -> return res'