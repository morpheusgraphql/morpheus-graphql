{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus
  ( interpreter
  , resolveTypes
  ) where

import           Control.Monad.Trans.Except          (ExceptT (..), runExceptT)
import           Data.Aeson                          (decode, encode)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as LB (ByteString, fromStrict, toStrict)
import           Data.Morpheus.Error.Utils           (errorMessage, renderErrors)
import           Data.Morpheus.Kind.GQLMutation      (GQLMutation (..))
import           Data.Morpheus.Kind.GQLQuery         (GQLQuery (..))
import           Data.Morpheus.Parser.Parser         (parseGQL, parseLineBreaks)
import           Data.Morpheus.PreProcess.PreProcess (preProcessQuery)
import           Data.Morpheus.Schema.Internal.Types (TypeLib)
import           Data.Morpheus.Types.Error           (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType          (JSType)
import           Data.Morpheus.Types.Query.Operator  (Operator (..))
import           Data.Morpheus.Types.Request         (GQLRequest)
import           Data.Morpheus.Types.Response        (GQLResponse (..))
import           Data.Morpheus.Types.Types           (GQLRoot (..))
import           Data.Proxy
import           Data.Text                           (Text, pack)
import qualified Data.Text                           as T (concat)
import qualified Data.Text.Lazy                      as LT (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding             (decodeUtf8, encodeUtf8)

schema :: (GQLQuery a, GQLMutation b) => a -> b -> TypeLib
schema queryRes mutationRes = mutationSchema mutationRes $ querySchema queryRes

resolve :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> GQLRequest -> ResolveIO JSType
resolve rootResolver body = do
  rootGQL <- ExceptT $ pure (parseGQL body >>= preProcessQuery gqlSchema)
  case rootGQL of
    Query _ _args selection _pos    -> encodeQuery queryRes gqlSchema selection
    Mutation _ _args selection _pos -> encodeMutation mutationRes selection
  where
    gqlSchema = schema queryRes mutationRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver

lineBreaks :: LB.ByteString -> [Int]
lineBreaks req =
  case decode req of
    Just x  -> parseLineBreaks x
    Nothing -> []

interpreterRaw :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> LB.ByteString -> IO GQLResponse
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
  interpreter :: (GQLQuery q, GQLMutation m) => GQLRoot q m -> a -> IO a

instance Interpreter LB.ByteString where
  interpreter root request = encode <$> interpreterRaw root request

instance Interpreter LT.Text where
  interpreter root request = decodeUtf8 <$> interpreter root (encodeUtf8 request)

instance Interpreter ByteString where
  interpreter root request = LB.toStrict <$> interpreter root (LB.fromStrict request)

instance Interpreter Text where
  interpreter root request = LT.toStrict <$> interpreter root (LT.fromStrict request)

type family GQL a :: *

data OBJECT =
  OBJECT

data SCALAR =
  SCALAR

class GQLObject a where
  introObject :: a -> Text
  introObject _ = "Object Of User"

class GQLScalar a where
  introScalar :: a -> Text
  introScalar _ = "Scalar Of Odd"

class Scanner a b where
  scan ::
       Proxy b
    -> (GQLScalar a =>
          a -> Text)
    -> (GQLObject a =>
          a -> Text)
    -> a
    -> Text

instance GQLScalar a => Scanner a SCALAR where
  scan _ c1 _ = c1

instance GQLObject a => Scanner a OBJECT where
  scan _ _ c2 = c2

newtype User =
  User Text

newtype Odd =
  Odd Int

instance GQLScalar Odd

type instance GQL Odd = SCALAR

instance GQLObject User

type instance GQL User = OBJECT

resolveUser :: Text
resolveUser = scan (Proxy @(GQL User)) introScalar introObject (User "")

resolveOdd :: Text
resolveOdd = scan (Proxy @(GQL Odd)) introScalar introObject (Odd 3)

resolveTypes :: Text
resolveTypes = T.concat [resolveOdd, resolveUser]
