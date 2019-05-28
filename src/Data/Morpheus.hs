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
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Lazy.Char8                as LB (ByteString, fromStrict, toStrict)
import           Data.Morpheus.Error.Utils                 (renderErrors)
import           Data.Morpheus.Kind.GQLMutation            (GQLMutation (..))
import           Data.Morpheus.Kind.GQLQuery               (GQLQuery (..))
import           Data.Morpheus.Kind.GQLSubscription        (GQLSubscription (..))
import           Data.Morpheus.Parser.Parser               (parseLineBreaks, parseRequest)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Internal.Data         (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation   (ResolveIO)
import           Data.Morpheus.Types.Internal.Value        (Value)
import           Data.Morpheus.Types.Response              (GQLResponse (..))
import           Data.Morpheus.Types.Types                 (GQLRoot (..))
import           Data.Morpheus.Validation.Validation       (validateRequest)
import           Data.Text                                 (Text)
import qualified Data.Text.Lazy                            as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding                   (decodeUtf8, encodeUtf8)

schema :: (GQLQuery a, GQLMutation b, GQLSubscription c) => a -> b -> c -> DataTypeLib
schema queryRes mutationRes subscriptionRes =
  subscriptionSchema subscriptionRes $ mutationSchema mutationRes $ querySchema queryRes

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
resolve :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> LB.ByteString -> ResolveIO Value {- Maybe Context -> -}
resolve rootResolver request {- context -}
 = do
  rootGQL <- ExceptT $ pure (parseRequest request >>= validateRequest gqlSchema)
  case rootGQL of
    Query operator'        -> encodeQuery queryRes gqlSchema $ operatorSelection operator'
    Mutation operator'     -> encodeMutation mutationRes $ operatorSelection operator'
    Subscription operator' -> encodeSubscription subscriptionRes $ operatorSelection operator'
  where
    gqlSchema = schema queryRes mutationRes subscriptionRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver
    subscriptionRes = subscription rootResolver

--type ConnectionID = Int
type Client c = (c, [Text])

data InputAction c a = SocketConnection
  { connectionID :: c
  , inputValue   :: a
  } deriving (Show)
  -- | NoEffectInput a

data OutputAction c a
  = Publish { actionChannelID  :: Text
            , actionPayload    :: a
            , mutationResponse :: a }
  | Subscribe { clientsState :: Client c }
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
  -> InputAction d Text
  -> ResolveIO (OutputAction d Value)
resolveStream rootResolver (SocketConnection id' request) = do
  rootGQL <- ExceptT $ pure (parseRequest (toLBS request) >>= validateRequest gqlSchema)
  case rootGQL of
    Query operator' -> do
      value <- encodeQuery queryRes gqlSchema $ operatorSelection operator'
      return (NoEffect value)
    Mutation operator' -> do
      value <- encodeMutation mutationRes $ operatorSelection operator'
      return Publish {actionChannelID = "UPDATE_ADDRESS", actionPayload = value, mutationResponse = value}
    Subscription operator' -> do
      _ <- encodeSubscription subscriptionRes $ operatorSelection operator'
      return Subscribe {clientsState = (id', ["UPDATE_ADDRESS"])}
  where
    gqlSchema = schema queryRes mutationRes subscriptionRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver
    subscriptionRes = subscription rootResolver

streamInterpreter ::
     (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> InputAction c Text -> IO (OutputAction c Text)
streamInterpreter rootResolver request = do
  value <- runExceptT (resolveStream rootResolver request)
  case value of
    Left x -> pure $ NoEffect $ encodeToText $ Errors $ renderErrors (parseLineBreaks $ toLBS $ inputValue request) x
    Right (Publish id' x' y') -> pure $ Publish id' (encodeToText $ Data x') (encodeToText $ Data y')
    Right (Subscribe x') -> pure $ Subscribe x'
    Right (NoEffect x') -> pure $ NoEffect (encodeToText $ Data x')

packStream :: (InputAction Int Text -> IO (OutputAction c Text)) -> LB.ByteString -> IO LB.ByteString
packStream streamAPI request = do
  value <- streamAPI (SocketConnection 0 $ bsToText request)
  case value of
    Publish {mutationResponse = res'} -> pure (toLBS res')
    Subscribe {}                      -> pure "subscriptions are only allowed in websocket"
    NoEffect res'                     -> pure (toLBS res')

interpreterRaw :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> LB.ByteString -> IO GQLResponse
interpreterRaw rootResolver request = do
  value <- runExceptT (resolve rootResolver request)
  case value of
    Left x  -> pure $ Errors $ renderErrors (parseLineBreaks request) x
    Right x -> pure $ Data x

class Interpreter a where
  interpreter :: (GQLQuery q, GQLMutation m, GQLSubscription s) => GQLRoot q m s -> a -> IO a

instance Interpreter LB.ByteString where
  interpreter root request = encode <$> interpreterRaw root request

instance Interpreter LT.Text where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter ByteString where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter Text where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)
