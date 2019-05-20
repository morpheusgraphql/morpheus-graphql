module Data.Morpheus
  ( interpreter
  ) where

import           Control.Monad.Trans.Except          (ExceptT (..), runExceptT)
import           Data.Aeson                          (decode, encode)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as LB (ByteString, fromStrict, toStrict)
import           Data.Morpheus.Error.Utils           (errorMessage, renderErrors)
import           Data.Morpheus.Kind.GQLMutation      (GQLMutation (..))
import           Data.Morpheus.Kind.GQLQuery         (GQLQuery (..))
import           Data.Morpheus.Kind.GQLSubscription  (GQLSubscription (..))
import           Data.Morpheus.Parser.Parser         (parseGQL, parseLineBreaks)
import           Data.Morpheus.Types.Error           (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Internal.AST    (ASTTypeLib)
import           Data.Morpheus.Types.Internal.Value  (Value)
import           Data.Morpheus.Types.Query.Operator  (Operator (..), Operator' (..))
import           Data.Morpheus.Types.Request         (GQLRequest)
import           Data.Morpheus.Types.Response        (GQLResponse (..))
import           Data.Morpheus.Types.Types           (GQLRoot (..))
import           Data.Morpheus.Validation.Validation (validateRequest)
import           Data.Text                           (Text, pack)
import qualified Data.Text.Lazy                      as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding             (decodeUtf8, encodeUtf8)

schema :: (GQLQuery a, GQLMutation b, GQLSubscription c) => a -> b -> c -> ASTTypeLib
schema queryRes mutationRes subscriptionRes =
  subscriptionSchema subscriptionRes $ mutationSchema mutationRes $ querySchema queryRes

resolve :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> GQLRequest -> ResolveIO Value
resolve rootResolver body = do
  rootGQL <- ExceptT $ pure (parseGQL body >>= validateRequest gqlSchema)
  case rootGQL of
    Query operator'        -> encodeQuery queryRes gqlSchema $ operatorSelection operator'
    Mutation operator'     -> encodeMutation mutationRes $ operatorSelection operator'
    Subscription operator' -> encodeSubscription subscriptionRes $ operatorSelection operator'
  where
    gqlSchema = schema queryRes mutationRes subscriptionRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver
    subscriptionRes = subscription rootResolver

lineBreaks :: LB.ByteString -> [Int]
lineBreaks req =
  case decode req of
    Just x  -> parseLineBreaks x
    Nothing -> []

interpreterRaw :: (GQLQuery a, GQLMutation b, GQLSubscription c) => GQLRoot a b c -> LB.ByteString -> IO GQLResponse
interpreterRaw rootResolver request = do
  value <- runExceptT $ parseRequest request >>= resolve rootResolver
  case value of
    Left x  -> pure $ Errors $ renderErrors (lineBreaks request) x
    Right x -> pure $ Data x

parseRequest :: LB.ByteString -> ResolveIO GQLRequest
parseRequest text =
  case decode text of
    Just x  -> pure x
    Nothing -> failResolveIO $ errorMessage 0 (pack $ show text)

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
