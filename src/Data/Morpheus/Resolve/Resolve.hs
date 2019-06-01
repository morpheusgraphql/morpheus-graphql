{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Resolve.Resolve
  ( interpreterRaw
  , streamInterpreter
  , schema
  , packStream
  , InputAction(..)
  , OutputAction(..)
  ) where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (encode)
import qualified Data.ByteString.Lazy.Char8                 as LB (ByteString)
import           Data.Morpheus.Error.Utils                  (renderErrors)
import           Data.Morpheus.Parser.Parser                (parseLineBreaks, parseRequest)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.GQLOperator            (GQLMutation (..), GQLQuery (..), GQLSubscription (..))
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value)
import           Data.Morpheus.Types.Internal.WebSocket     (InputAction (..), OutputAction (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Morpheus.Types.Types                  (GQLRoot (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.Text                                  (Text)
import qualified Data.Text.Lazy                             as LT (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                    (decodeUtf8, encodeUtf8)
import           Data.UUID.V4                               (nextRandom)

toLBS :: Text -> LB.ByteString
toLBS = encodeUtf8 . LT.fromStrict

bsToText :: LB.ByteString -> Text
bsToText = LT.toStrict . decodeUtf8

encodeToText :: GQLResponse -> Text
encodeToText = bsToText . encode

resolveStream ::
     (GQLQuery a, GQLMutation b, GQLSubscription c)
  => GQLRoot a b c
  -> InputAction Text
  -> ResolveIO (OutputAction Value)
resolveStream rootResolver (SocketInput id' request) =
  (ExceptT $ pure (parseRequest (toLBS request) >>= validateRequest gqlSchema)) >>= resolve
  where
    resolve (Query operator') = do
      value <- encodeQuery gqlSchema queryRes $ operatorSelection operator'
      return (NoEffect value)
    resolve (Mutation operator') = do
      WithEffect channels value <- encodeMutation mutationRes $ operatorSelection operator'
      return PublishMutation {mutationChannels = channels, mutationResponse = value, subscriptionResolver = sRes}
      where
        sRes :: SelectionSet -> IO Text
        sRes selection' = do
          value <- runExceptT (encodeSubscription subscriptionRes selection')
          case value of
            Left x -> pure $ encodeToText $ Errors $ renderErrors (parseLineBreaks $ toLBS request) x
            Right (WithEffect _ x') -> pure (encodeToText $ Data x')
    resolve (Subscription operator') = do
      WithEffect channels _ <- encodeSubscription subscriptionRes $ operatorSelection operator'
      return
        InitSubscription
          {subscriptionClientID = id', subscriptionChannels = channels, subscriptionQuery = operatorSelection operator'}
    gqlSchema = schema queryRes mutationRes subscriptionRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver
    subscriptionRes = subscription rootResolver

streamInterpreter ::
     (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> InputAction Text -> IO (OutputAction Text)
streamInterpreter rootResolver request = do
  value <- runExceptT (resolveStream rootResolver request)
  -- print value
  case value of
    Left x -> pure $ NoEffect $ encodeToText $ Errors $ renderErrors (parseLineBreaks $ toLBS $ inputValue request) x
    Right (PublishMutation id' x' y') -> pure $ PublishMutation id' (encodeToText $ Data x') y'
    Right (InitSubscription x' y' z') -> pure $ InitSubscription x' y' z'
    Right (NoEffect x') -> pure $ NoEffect (encodeToText $ Data x')

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

{-- Actual response-}
schema :: (GQLQuery a, GQLMutation b, GQLSubscription c) => a -> b -> c -> DataTypeLib
schema queryRes mutationRes subscriptionRes =
  subscriptionSchema subscriptionRes $ mutationSchema mutationRes $ querySchema queryRes

interpreterRaw :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> LB.ByteString -> IO GQLResponse
interpreterRaw rootResolver request = do
  value <- runExceptT resolve
  case value of
    Left x  -> pure $ Errors $ renderErrors (parseLineBreaks request) x
    Right x -> pure $ Data x
  where
    resolve = do
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
