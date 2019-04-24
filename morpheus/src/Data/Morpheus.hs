{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus
  ( interpreter
  , interpreterBytestring
  , GQLResponse
  , (::->)(..)
  , ResolveIO
  , EnumOf(unpackEnum)
  , GQLRoot(..)
  , ScalarOf(..)
  ) where

import           Control.Monad.Trans.Except          (ExceptT (..), runExceptT)
import           Data.Aeson                          (decode, encode)
import qualified Data.ByteString.Lazy.Char8          as B
import           Data.Morpheus.Error.Utils           (errorMessage, renderErrors)
import           Data.Morpheus.Kind.GQLMutation      (GQLMutation (..))
import           Data.Morpheus.Kind.GQLQuery         (GQLQuery (..))
import           Data.Morpheus.Parser.Parser         (parseGQL, parseLineBreaks)
import           Data.Morpheus.PreProcess.PreProcess (preProcessQuery)
import           Data.Morpheus.Schema.Internal.Types (TypeLib)
import           Data.Morpheus.Types.Describer       ((::->) (Resolver), EnumOf (unpackEnum), ScalarOf (..))
import           Data.Morpheus.Types.Error           (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType          (JSType)
import           Data.Morpheus.Types.Query.Operator  (Operator (..))
import           Data.Morpheus.Types.Request         (GQLRequest)
import           Data.Morpheus.Types.Response        (GQLResponse (..))
import           Data.Text                           (pack)

data GQLRoot a b = GQLRoot
  { query    :: a
  , mutation :: b
  }

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

lineBreaks :: B.ByteString -> [Int]
lineBreaks req =
  case decode req of
    Just x  -> parseLineBreaks x
    Nothing -> []

interpreter :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> B.ByteString -> IO GQLResponse
interpreter rootResolver request = do
  value <- runExceptT $ parseRequest request >>= resolve rootResolver
  case value of
    Left x  -> pure $ Errors $ renderErrors (lineBreaks request) x
    Right x -> pure $ Data x

interpreterBytestring :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> B.ByteString -> IO B.ByteString
interpreterBytestring rootResolver request = encode <$> interpreter rootResolver request

parseRequest :: B.ByteString -> ResolveIO GQLRequest
parseRequest text =
  case decode text of
    Just x  -> pure x
    Nothing -> failResolveIO $ errorMessage 0 (pack $ show text)
