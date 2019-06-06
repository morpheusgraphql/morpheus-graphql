{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Resolve.Resolve
  ( resolve
  , resolveStream
  , packStream
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (encode)
import qualified Data.ByteString.Lazy.Char8                 as LB (ByteString)
import           Data.Morpheus.Error.Utils                  (renderErrors)
import           Data.Morpheus.Parser.Parser                (parseRequest)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.GQLOperator            (GQLMutation (..), GQLQuery (..), GQLSubscription (..))
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataTypeLib)
import           Data.Morpheus.Types.Internal.WebSocket     (InputAction (..), OutputAction (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Morpheus.Types.Types                  (GQLRoot (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.Text                                  (Text)
import qualified Data.Text.Lazy                             as LT (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                    (decodeUtf8, encodeUtf8)
import           Data.UUID.V4                               (nextRandom)

schema :: (GQLQuery a, GQLMutation b, GQLSubscription c) => a -> b -> c -> DataTypeLib
schema queryRes mutationRes subscriptionRes =
  subscriptionSchema subscriptionRes $ mutationSchema mutationRes $ querySchema queryRes

toLBS :: Text -> LB.ByteString
toLBS = encodeUtf8 . LT.fromStrict

bsToText :: LB.ByteString -> Text
bsToText = LT.toStrict . decodeUtf8

encodeToText :: GQLResponse -> Text
encodeToText = bsToText . encode

resolve :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> LB.ByteString -> IO GQLResponse
resolve rootResolver request = do
  value <- runExceptT _resolve
  case value of
    Left x  -> pure $ Errors $ renderErrors x
    Right x -> pure $ Data x
  where
    _resolve = do
      rootGQL <- ExceptT $ pure (parseRequest request >>= validateRequest gqlSchema)
      case rootGQL of
        Query operator'        -> encodeQuery gqlSchema queryRes $ operatorSelection operator'
        Mutation operator'     -> resultValue <$> encodeMutation mutationRes (operatorSelection operator')
        Subscription operator' -> resultValue <$> encodeSubscription subscriptionRes (operatorSelection operator')
      where
        gqlSchema = schema queryRes mutationRes subscriptionRes
        queryRes = query rootResolver
        mutationRes = mutation rootResolver
        subscriptionRes = subscription rootResolver

resolveStream ::
     (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> InputAction Text -> IO (OutputAction Text)
resolveStream rootResolver (SocketInput id' request) = do
  value <- runExceptT _resolve
  case value of
    Left x                             -> pure $ NoEffect $ encodeToText $ Errors $ renderErrors x
    Right (PublishMutation pid' x' y') -> pure $ PublishMutation pid' (encodeToText $ Data x') y'
    Right (InitSubscription x' y' z')  -> pure $ InitSubscription x' y' z'
    Right (NoEffect x')                -> pure $ NoEffect (encodeToText $ Data x')
  where
    _resolve = (ExceptT $ pure (parseRequest (toLBS request) >>= validateRequest gqlSchema)) >>= resolveOperator
      where
        resolveOperator (Query operator') = do
          value <- encodeQuery gqlSchema queryRes $ operatorSelection operator'
          return (NoEffect value)
        resolveOperator (Mutation operator') = do
          WithEffect channels value <- encodeMutation mutationRes $ operatorSelection operator'
          return PublishMutation {mutationChannels = channels, mutationResponse = value, subscriptionResolver = sRes}
          where
            sRes :: SelectionSet -> IO Text
            sRes selection' = do
              value <- runExceptT (encodeSubscription subscriptionRes selection')
              case value of
                Left x                  -> pure $ encodeToText $ Errors $ renderErrors x
                Right (WithEffect _ x') -> pure (encodeToText $ Data x')
        resolveOperator (Subscription operator') = do
          WithEffect channels _ <- encodeSubscription subscriptionRes $ operatorSelection operator'
          return
            InitSubscription
              { subscriptionClientID = id'
              , subscriptionChannels = channels
              , subscriptionQuery = operatorSelection operator'
              }
        gqlSchema = schema queryRes mutationRes subscriptionRes
        queryRes = query rootResolver
        mutationRes = mutation rootResolver
        subscriptionRes = subscription rootResolver

packStream :: GQLState -> (InputAction Text -> IO (OutputAction Text)) -> LB.ByteString -> IO LB.ByteString
packStream state streamAPI request = do
  id' <- nextRandom
  value <- streamAPI (SocketInput id' $ bsToText request)
  case value of
    PublishMutation {mutationChannels = channels, mutationResponse = res', subscriptionResolver = resolver'} -> do
      publishUpdates channels resolver' state
      pure (toLBS res')
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoEffect res' -> pure (toLBS res')
