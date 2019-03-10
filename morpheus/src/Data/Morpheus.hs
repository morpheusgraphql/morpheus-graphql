{-# LANGUAGE TypeOperators , FlexibleInstances  #-}

module Data.Morpheus
  ( interpreter
  , eitherToResponse
  , GQLResponse
  , GQLSelection
  , GQLQuery
  , GQLArgs
  , (::->)(..)
  , GQLRequest(..)
  , ResolveIO(..)
  , GQLInput
  , EnumOf(unpackEnum)
  , GQLEnum
  , GQLRoot(..)
  , GQLMutation(..)
  , NoMutation(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Morpheus.Generics.GQLSelection
                                                ( GQLSelection )
import           Data.Morpheus.Generics.GQLQuery
                                                ( GQLQuery(..) )
import           Data.Morpheus.Generics.GQLArgs ( GQLArgs )
import           Data.Morpheus.Parser.Parser    ( parseGQL )
import           Data.Morpheus.Types.JSType     ( JSType )
import           Data.Morpheus.Types.Types      ( (::->)(Resolver)
                                                , GQLResponse(..)
                                                , GQLRequest(..)
                                                , Validation(..)
                                                , ResolveIO(..)
                                                , failResolveIO
                                                , EnumOf(unpackEnum)
                                                , GQLOperator(..)
                                                , GQLQueryRoot(..)
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.Morpheus.ErrorMessage     ( errorMessage )
import           Control.Monad.Trans.Except     ( runExceptT
                                                , ExceptT(..)
                                                )
import           Data.Morpheus.Generics.GQLInput
                                                ( GQLInput )
import           Data.Morpheus.Generics.GQLEnum ( GQLEnum )
import           Data.Morpheus.Generics.GQLMutation
                                                ( GQLMutation(..)
                                                , NoMutation(..)
                                                )
import           Data.Morpheus.Types.Introspection
                                                ( GQLTypeLib )
import           Data.Morpheus.PreProcess.PreProcess
                                                ( preProcessQuery )
import           Data.Aeson                     ( decode )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe                     ( fromMaybe )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.Error.Utils      ( renderErrors )


data GQLRoot a b = GQLRoot {
  queryResolver :: a,
  mutationResolver:: b
}

schema :: (GQLQuery a, GQLMutation b) => a -> b -> GQLTypeLib
schema query mutation = querySchema query $ mutationSchema mutation

validate schema root = case preProcessQuery schema root of
  Right validGQL -> pure validGQL
  Left  x        -> failResolveIO x

resolve
  :: (GQLQuery a, GQLMutation b)
  => GQLRoot a b
  -> GQLRequest
  -> ResolveIO JSType
resolve rootResolver body = do
  rootGQL <- ExceptT $ pure (parseGQL body >>= preProcessQuery gqlSchema)
  case rootGQL of
    QueryOperator name query -> encodeQuery queryRes gqlSchema query
    MutationOperator name mutation ->
      encodeMutation mutationRes gqlSchema mutation
 where
  gqlSchema   = schema queryRes mutationRes
  queryRes    = queryResolver rootResolver
  mutationRes = mutationResolver rootResolver

interpreter
  :: (GQLQuery a, GQLMutation b)
  => GQLRoot a b
  -> B.ByteString
  -> IO GQLResponse
interpreter rootResolver request = do
  value <- runExceptT $ parseRequest request >>= resolve rootResolver
  case value of
    Left  x -> pure $ Errors $ renderErrors x
    Right x -> pure $ Data x

eitherToResponse :: (a -> a) -> Either String a -> ResolveIO a
eitherToResponse f (Left  x) = failResolveIO $ errorMessage [] 0 (pack $ show x)
eitherToResponse f (Right x) = pure (f x)

parseRequest :: B.ByteString -> ResolveIO GQLRequest
parseRequest text = case decode text of
  Just x  -> pure x
  Nothing -> failResolveIO $ errorMessage [] 0 (pack $ show text)
