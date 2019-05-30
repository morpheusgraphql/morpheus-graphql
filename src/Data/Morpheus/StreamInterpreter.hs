{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Morpheus.StreamInterpreter where

import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT)
import           Data.Aeson                                 (encode)
import qualified Data.ByteString.Lazy.Char8                 as LB (ByteString)
import           Data.Morpheus.Error.Utils                  (renderErrors)
import           Data.Morpheus.Interpreter                  (schema)
import           Data.Morpheus.Kind.GQLOperator             (GQLMutation (..), GQLQuery (..), GQLSubscription (..))
import           Data.Morpheus.Parser.Parser                (parseLineBreaks, parseRequest)
import           Data.Morpheus.Server.ClientRegister        (GQLState, publishUpdates)
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value)
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Morpheus.Types.Response               (GQLResponse (..))
import           Data.Morpheus.Types.Types                  (GQLRoot (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)
import           Data.Text                                  (Text)
import qualified Data.Text.Lazy                             as LT (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                    (decodeUtf8, encodeUtf8)

data InputAction a = SocketInput
  { connectionID :: Int
  , inputValue   :: a
  } deriving (Show)

data OutputAction a
  = PublishMutation { mutationChannels     :: [Text]
                    , mutationResponse     :: a
                    , subscriptionResolver :: SelectionSet -> IO Text }
  | InitSubscription { subscriptionClientID :: Int
                     , subscriptionChannels :: [Text]
                     , subscriptionQuery    :: SelectionSet }
  | NoEffect a

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
  value <- streamAPI (SocketInput 0 $ bsToText request)
  case value of
    PublishMutation {mutationChannels = channels, mutationResponse = res', subscriptionResolver = resolver'} -> do
      publishUpdates channels resolver' state
      pure (toLBS res')
{-- Actual response-}
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoEffect res' -> pure (toLBS res')
