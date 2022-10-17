{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Internal
  ( FromJSON (..),
    ToJSON (..),
    RequestType (..),
    Generic,
    OperationType (..),
    scalarFromJSON,
    scalarToJSON,
    invalidConstructorError,
    omitNulls,
    (.=),
    withObject,
    (.:),
    (.:?),
    takeValueType,
  )
where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser, Value (..))
import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Internal.Utils (IsMap (lookup))
import Data.Morpheus.Types.GQLScalar (scalarFromJSON, scalarToJSON)
import Data.Morpheus.Types.Internal.AST (OperationType (..))
import qualified Data.Text as T
import Relude

invalidConstructorError :: (MonadFail m, Show a) => a -> m b
invalidConstructorError v = fail $ show v <> " is Not Valid Union Constructor"

takeValueType :: ((String, Object) -> Parser a) -> Value -> Parser a
takeValueType f (Object hMap) = case lookup "__typename" hMap of
  Nothing -> fail "key \"__typename\" not found on object"
  Just (String x) -> f (T.unpack x, hMap)
  Just val ->
    fail $ "key \"__typename\" should be string but found: " <> show val
takeValueType _ _ = fail "expected Object"

omitNulls :: [Pair] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True
