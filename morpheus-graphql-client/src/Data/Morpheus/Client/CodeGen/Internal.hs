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
    withUnion,
    omitNulls,
    (.=),
    withObject,
    (.:),
    (.:?),
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
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

withUnion :: ((String, Value) -> Parser a) -> Value -> Parser a
withUnion f v@(Object hMap) = case lookup "__typename" hMap of
  Nothing -> f ("__TYPENAME_NOT__FOUND__", v)
  Just (String x) -> f (T.unpack x, v)
  Just val ->
    fail $ "key \"__typename\" should be string but found: " <> show val
withUnion _ _ = fail "expected Object"

omitNulls :: [Pair] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True
