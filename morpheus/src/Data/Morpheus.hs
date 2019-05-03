{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, DeriveAnyClass,
  InstanceSigs #-}

module Data.Morpheus
  ( interpreter
  , resolveTypes
  ) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, fromStrict, toStrict)
import Data.Morpheus.Error.Utils (errorMessage, renderErrors)
import Data.Morpheus.Kind.GQLMutation (GQLMutation(..))
import Data.Morpheus.Kind.GQLQuery (GQLQuery(..))
import Data.Morpheus.Parser.Parser (parseGQL, parseLineBreaks)
import Data.Morpheus.PreProcess.PreProcess (preProcessQuery)
import Data.Morpheus.Schema.Internal.Types (TypeLib)
import Data.Morpheus.Types.Error (ResolveIO, failResolveIO)
import Data.Morpheus.Types.JSType (JSType)
import Data.Morpheus.Types.Query.Operator (Operator(..))
import Data.Morpheus.Types.Request (GQLRequest)
import Data.Morpheus.Types.Response (GQLResponse(..))
import Data.Morpheus.Types.Types (GQLRoot(..))
import Data.Proxy
import Data.Text (Text, pack)
import qualified Data.Text as T (concat)
import qualified Data.Text.Lazy as LT (Text, fromStrict, toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import GHC.Exts

schema :: (GQLQuery a, GQLMutation b) => a -> b -> TypeLib
schema queryRes mutationRes = mutationSchema mutationRes $ querySchema queryRes

resolve :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> GQLRequest -> ResolveIO JSType
resolve rootResolver body = do
  rootGQL <- ExceptT $ pure (parseGQL body >>= preProcessQuery gqlSchema)
  case rootGQL of
    Query _ _args selection _pos -> encodeQuery queryRes gqlSchema selection
    Mutation _ _args selection _pos -> encodeMutation mutationRes selection
  where
    gqlSchema = schema queryRes mutationRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver

lineBreaks :: LB.ByteString -> [Int]
lineBreaks req =
  case decode req of
    Just x -> parseLineBreaks x
    Nothing -> []

interpreterRaw :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> LB.ByteString -> IO GQLResponse
interpreterRaw rootResolver request = do
  value <- runExceptT $ parseRequest request >>= resolve rootResolver
  print resolveTypes
  case value of
    Left x -> pure $ Errors $ renderErrors (lineBreaks request) x
    Right x -> pure $ Data x

parseRequest :: LB.ByteString -> ResolveIO GQLRequest
parseRequest text =
  case decode text of
    Just x -> pure x
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

-- Hidden GQL type System API
type family GQL a :: *

data OBJECT

data SCALAR

type family GQLConstraint a b :: Constraint

class IntrospectionRouter a b where
  routeIntrospection :: Proxy b -> a -> Text

introspection ::
     forall a. IntrospectionRouter a (GQL a)
  => a
  -> Text
introspection = routeIntrospection (Proxy @(GQL a))

-- Define GQL Kind Classes
-- Define and bind GQLObject
class GQLObject a where
  introspectObject :: (Show a, Eq a) => a -> Text
  introspectObject x = T.concat ["Resolved Object :: ", pack $ show x]

type instance GQLConstraint a OBJECT = GQLObject a

instance (GQLObject a, Eq a, Show a) => IntrospectionRouter a OBJECT where
  routeIntrospection _ = introspectObject

-- Define and bind GQLScalar
class GQLScalar a where
  introspectScalar :: (Show a, Read a) => a -> Text
  introspectScalar x = T.concat ["Resolved Scalar:: ", pack $ show x]

type instance GQLConstraint a SCALAR = GQLScalar a

instance (GQLScalar a, Read a, Show a) => IntrospectionRouter a SCALAR where
  routeIntrospection _ = introspectScalar

-- Client Side
-- define GQL Schema
newtype User =
  User Text
  deriving (Show, Eq, GQLObject)

newtype Odd =
  Odd Int
  deriving (Show, Read, GQLScalar)

type instance GQL Odd = SCALAR

type instance GQL User = OBJECT

-- Resolvers
resolveTypes :: Text
resolveTypes = T.concat [introspection (Odd 3), " and ", introspection (User "David")]