{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Resolve.Resolve
  ( resolve
  , resolveByteString
  , resolveStreamByteString
  , resolveStream
  , packStream
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (eitherDecode, encode)
import           Data.ByteString.Lazy.Char8                 (ByteString)
import           Data.Morpheus.Error.Utils                  (badRequestError, renderErrors)
import           Data.Morpheus.Parser.Parser                (parseGQL)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.GQLOperator            (RootResCon, effectEncode, encodeQuery, fullSchema)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.WebSocket     (OutputAction (..))
import           Data.Morpheus.Types.Request                (GQLRequest (..))
import           Data.Morpheus.Types.Resolver               (unpackEffect, unpackEffect2)
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Morpheus.Types.Types                  (GQLRootResolver (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)

resolveByteString :: RootResCon IO q m s => GQLRootResolver q m s -> ByteString -> IO ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolve rootResolver req

resolveStreamByteString :: RootResCon IO q m s => GQLRootResolver q m s -> ByteString -> IO (OutputAction ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoEffect $ badRequestError aesonError'
    Right req        -> fmap encode <$> resolveStream rootResolver req

resolve :: RootResCon IO a b c => GQLRootResolver a b c -> GQLRequest -> IO GQLResponse
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
                  snd <$> unpackEffect2 (effectEncode mutationResolver (operatorSelection operator'))
                Subscription operator' ->
                  snd <$> unpackEffect2 (effectEncode subscriptionResolver (operatorSelection operator'))

resolveStream :: RootResCon IO q m s => GQLRootResolver q m s -> GQLRequest -> IO (OutputAction GQLResponse)
resolveStream GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver} request =
  case fullSchema queryResolver mutationResolver subscriptionResolver of
    Left error' -> return $ NoEffect $ Errors $ renderErrors error'
    Right validSchema -> do
      value <- runExceptT $ _resolve validSchema
      case value of
        Left x       -> return $ NoEffect $ Errors $ renderErrors x
        Right value' -> return $ fmap Data value'
  where
    _resolve gqlSchema = (ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)) >>= resolveOperator
      where
        resolveOperator (Query operator') = do
          value <- encodeQuery gqlSchema queryResolver $ operatorSelection operator'
          return (NoEffect value)
        resolveOperator (Mutation operator') = do
          (channels, response) <- unpackEffect2 $ effectEncode mutationResolver $ operatorSelection operator'
          return
            PublishMutation {mutationChannels = channels, mutationResponse = response, currentSubscriptionStateResolver}
          where
            currentSubscriptionStateResolver :: SelectionSet -> IO GQLResponse
            currentSubscriptionStateResolver selection' = do
              value <- unpackEffect (effectEncode subscriptionResolver selection')
              case value of
                Left x        -> pure $ Errors $ renderErrors x
                Right (_, x') -> return $ Data x'
        resolveOperator (Subscription operator') = do
          (subscriptionChannels, _) <- unpackEffect2 $ effectEncode subscriptionResolver $ operatorSelection operator'
          return InitSubscription {subscriptionChannels, subscriptionQuery = operatorSelection operator'}

packStream :: GQLState -> (ByteString -> IO (OutputAction ByteString)) -> ByteString -> IO ByteString
packStream state streamAPI request = do
  value <- streamAPI request
  case value of
    PublishMutation {mutationChannels, mutationResponse, currentSubscriptionStateResolver} -> do
      publishUpdates mutationChannels currentSubscriptionStateResolver state
      return mutationResponse
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoEffect res' -> return res'
