{-# LANGUAGE RankNTypes #-}

module Data.Morpheus
  ( interpreter
  , GQLHandler
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
-}
resolve :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> LB.ByteString -> ResolveIO Value
resolve rootResolver request = do
  rootGQL <- ExceptT $ pure (parseRequest request >>= validateRequest gqlSchema)
  case rootGQL of
    Query operator'        -> encodeQuery queryRes gqlSchema $ operatorSelection operator'
    Mutation operator'     -> encodeMutation mutationRes $ operatorSelection operator'
    {-
       TODO: setup stream
       Mutation returns -> [UpdateAction]
       that can be resolved by subscriptions
    -}
    Subscription operator' -> encodeSubscription subscriptionRes $ operatorSelection operator'
     {-
           TODO: setup stream
           Subscription returns -> [UpdateAction]

          subscriptionResponse:: [(UpdateAction,Connection)] -> [UpdateAction] -> Stream GQLResponse
          subscriptionResponse subscriptionActions mutationActions = do
               matchedActions <- match mutationActions subscriptionActions  -- only Subscribed UpdateActions will be executed
               mapM sendResponse matchedActions  -- all subscribed actions will be send
           that can be resolved by subscriptions
     -}
  where
    gqlSchema = schema queryRes mutationRes subscriptionRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver
    subscriptionRes = subscription rootResolver

type GQLRootResolver d
   = forall a b c. (GQLQuery a, GQLMutation b, GQLSubscription c) =>
                     GQLRoot a b c -> d

type GQLHandler a
   = Interpreter a =>
       a -> IO a

interpreterRaw :: GQLRootResolver (LB.ByteString -> IO GQLResponse)
interpreterRaw rootResolver request = do
  value <- runExceptT (resolve rootResolver request)
  case value of
    Left x  -> pure $ Errors $ renderErrors (parseLineBreaks request) x
    Right x -> pure $ Data x

class Interpreter a where
  interpreter :: GQLRootResolver (a -> IO a)

instance Interpreter LB.ByteString where
  interpreter root request = encode <$> interpreterRaw root request

instance Interpreter LT.Text where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter ByteString where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter Text where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)
