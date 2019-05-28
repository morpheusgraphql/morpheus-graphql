{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Morpheus
  ( interpreter
  , streamInterpreter
  , packStream
  , InputAction(..)
  , OutputAction(..)
  ) where

import           Control.Monad.Trans.Except                (ExceptT (..), runExceptT)
import           Data.Aeson                                (encode)
import qualified Data.ByteString.Lazy.Char8                as LB (ByteString)
import           Data.Morpheus.Error.Utils                 (renderErrors)
import           Data.Morpheus.Interpreter                 (Interpreter (..), schema)
import           Data.Morpheus.Kind.GQLMutation            (GQLMutation (..))
import           Data.Morpheus.Kind.GQLQuery               (GQLQuery (..))
import           Data.Morpheus.Kind.GQLSubscription        (GQLSubscription (..))
import           Data.Morpheus.Parser.Parser               (parseLineBreaks, parseRequest)
import           Data.Morpheus.Server.ClientRegister       (GQLState, publishUpdates)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.Validation   (ResolveIO)
import           Data.Morpheus.Types.Internal.Value        (Value)
import           Data.Morpheus.Types.Response              (GQLResponse (..))
import           Data.Morpheus.Types.Types                 (GQLRoot (..))
import           Data.Morpheus.Validation.Validation       (validateRequest)
import           Data.Text                                 (Text)
import qualified Data.Text.Lazy                            as LT (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                   (decodeUtf8, encodeUtf8)

{-
  data UpdateAction = UserAdded (Async User) | AddressAdded (Async Address)

  data Subscription = Subscription {
      newUser :: Async User,
      newAddress :: Async Address
  }

  -- resolveNewUserSubscription :: Async User
  -- resolveSubscription = async (UserAdded Pending)

  async :: Channels -> Stream Channels
  async = ....

  newtype Async a = Pending | Response { unpackAwait :: Stream Channels } |

  data Context = Context {
      allConnections :: [(UpdateAction,Connection)]
  }
-}
-- hied should be add WebSocket Connection :: Maybe Context)
data InputAction a = SocketInput
  { connectionID :: Int
  , inputValue   :: a
  } deriving (Show)

data OutputAction a
  = PublishMutation { mutationChannelID :: Text
                    , mutationPayload   :: a
                    , mutationResponse  :: a }
  | InitSubscription { subscriptionClientID :: Int
                     , subscriptionChannels :: [Text] }
  | NoEffect a
  deriving (Show)

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
resolveStream rootResolver (SocketInput id' request) = do
  rootGQL <- ExceptT $ pure (parseRequest (toLBS request) >>= validateRequest gqlSchema)
  case rootGQL of
    Query operator' -> do
      value <- encodeQuery queryRes gqlSchema $ operatorSelection operator'
      return (NoEffect value)
    Mutation operator' -> do
      value <- encodeMutation mutationRes $ operatorSelection operator'
      return PublishMutation {mutationChannelID = "UPDATE_ADDRESS", mutationPayload = value, mutationResponse = value}
    Subscription operator' -> do
      _ <- encodeSubscription subscriptionRes $ operatorSelection operator'
      return InitSubscription {subscriptionClientID = id', subscriptionChannels = ["UPDATE_ADDRESS"]}
  where
    gqlSchema = schema queryRes mutationRes subscriptionRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver
    subscriptionRes = subscription rootResolver

streamInterpreter ::
     (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> InputAction Text -> IO (OutputAction Text)
streamInterpreter rootResolver request = do
  value <- runExceptT (resolveStream rootResolver request)
  case value of
    Left x -> pure $ NoEffect $ encodeToText $ Errors $ renderErrors (parseLineBreaks $ toLBS $ inputValue request) x
    Right (PublishMutation id' x' y') -> pure $ PublishMutation id' (encodeToText $ Data x') (encodeToText $ Data y')
    Right (InitSubscription x' y') -> pure $ InitSubscription x' y'
    Right (NoEffect x') -> pure $ NoEffect (encodeToText $ Data x')

packStream :: GQLState -> (InputAction Text -> IO (OutputAction Text)) -> LB.ByteString -> IO LB.ByteString
packStream state streamAPI request = do
  value <- streamAPI (SocketInput 0 $ bsToText request)
  case value of
    PublishMutation {mutationChannelID = id', mutationResponse = res', mutationPayload = message'} -> do
      publishUpdates id' message' state
      pure (toLBS res') {-- Actual response-}
    InitSubscription {} -> pure "subscriptions are only allowed in websocket"
    NoEffect res' -> pure (toLBS res')
