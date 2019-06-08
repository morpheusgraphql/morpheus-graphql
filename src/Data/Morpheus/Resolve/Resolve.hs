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
import           Data.Morpheus.Types.GQLOperator            (GQLMutation (..), GQLQuery (..), GQLSubscription (..))
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataTypeLib)
import           Data.Morpheus.Types.Internal.WebSocket     (OutputAction (..))
import qualified Data.Morpheus.Types.Request                as Req (GQLRequest (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Morpheus.Types.Types                  (GQLRoot (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)

schema :: (GQLQuery a, GQLMutation b, GQLSubscription c) => a -> b -> c -> DataTypeLib
schema queryRes mutationRes subscriptionRes =
  subscriptionSchema subscriptionRes $ mutationSchema mutationRes $ querySchema queryRes

resolveByteString :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> ByteString -> IO ByteString
resolveByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ badRequestError aesonError'
    Right req        -> encode <$> resolve rootResolver req

resolveStreamByteString ::
     (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> ByteString -> IO (OutputAction ByteString)
resolveStreamByteString rootResolver request =
  case eitherDecode request of
    Left aesonError' -> return $ NoEffect $ badRequestError aesonError'
    Right req        -> fmap encode <$> resolveStream rootResolver req

resolve :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> Req.GQLRequest -> IO GQLResponse
resolve GQLRoot {queryResolver = queryRes, mutationResolver = mutationRes, subscriptionResolver = subscriptionRes} request = do
  value <- runExceptT _resolve
  case value of
    Left x  -> pure $ Errors $ renderErrors x
    Right x -> pure $ Data x
  where
    _resolve = do
      rootGQL <- ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)
      case rootGQL of
        Query operator'        -> encodeQuery gqlSchema queryRes $ operatorSelection operator'
        Mutation operator'     -> resultValue <$> encodeMutation mutationRes (operatorSelection operator')
        Subscription operator' -> resultValue <$> encodeSubscription subscriptionRes (operatorSelection operator')
      where
        gqlSchema = schema queryRes mutationRes subscriptionRes

resolveStream ::
     (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> Req.GQLRequest -> IO (OutputAction GQLResponse)
resolveStream GQLRoot {queryResolver = queryRes, mutationResolver = mutationRes, subscriptionResolver = subscriptionRes} request = do
  value <- runExceptT _resolve
  case value of
    Left x       -> pure $ NoEffect $ Errors $ renderErrors x
    Right value' -> return $ fmap Data value'
  where
    _resolve = (ExceptT $ pure (parseGQL request >>= validateRequest gqlSchema)) >>= resolveOperator
      where
        resolveOperator (Query operator') = do
          value <- encodeQuery gqlSchema queryRes $ operatorSelection operator'
          return (NoEffect value)
        resolveOperator (Mutation operator') = do
          WithEffect channels value <- encodeMutation mutationRes $ operatorSelection operator'
          return
            PublishMutation
              {mutationChannels = channels, mutationResponse = value, currentSubscriptionStateResolver = sRes}
          where
            sRes :: SelectionSet -> IO GQLResponse
            sRes selection' = do
              value <- runExceptT (encodeSubscription subscriptionRes selection')
              case value of
                Left x                  -> pure $ Errors $ renderErrors x
                Right (WithEffect _ x') -> pure $ Data x'
        resolveOperator (Subscription operator') = do
          WithEffect channels _ <- encodeSubscription subscriptionRes $ operatorSelection operator'
          return InitSubscription {subscriptionChannels = channels, subscriptionQuery = operatorSelection operator'}
        gqlSchema = schema queryRes mutationRes subscriptionRes

packStream :: GQLState -> (ByteString -> IO (OutputAction ByteString)) -> ByteString -> IO ByteString
packStream state streamAPI request = do
  value <- streamAPI request
  case value of
    PublishMutation {mutationChannels = channels, mutationResponse = res', currentSubscriptionStateResolver = resolver'} -> do
      publishUpdates channels resolver' state
      return res'
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoEffect res' -> return res'
