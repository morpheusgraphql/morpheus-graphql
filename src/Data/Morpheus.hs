{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus
    ( interpreter
    , GQLResponse
    , GQLSelection
    , GQLQuery
    , GQLArgs
    , (::->)(..)
    , GQLRequest(..)
    , eitherToResponse
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
import           Data.Morpheus.Generics.GQLArgs
                                                ( GQLArgs )
import           Data.Morpheus.Parser.Parser   ( parseGQL )
import           Data.Morpheus.Types.JSType     (JSType)
import           Data.Morpheus.Types.Types     ( (::->)(Resolver)
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
import           Data.Morpheus.ErrorMessage    ( errorMessage )
import           Control.Monad.Trans.Except     ( runExceptT
                                                , ExceptT(..)
                                                )
import          Data.Morpheus.Generics.GQLInput (GQLInput)
import          Data.Morpheus.Generics.GQLEnum  (GQLEnum)
import          Data.Morpheus.Generics.GQLMutation (GQLMutation(..),NoMutation(..))
import          Data.Morpheus.Types.Introspection (GQLTypeLib)
import           Data.Morpheus.PreProcess       ( preProcessQuery )


data GQLRoot a b = GQLRoot {
  queryResolver :: a,
  mutationResolver:: b
}

schema :: ( GQLQuery a , GQLMutation b ) =>  a -> b -> GQLTypeLib
schema query mutation  = querySchema query $ mutationSchema mutation

validate schema root = case preProcessQuery schema root of
        Right validGQL -> pure  validGQL
        Left x ->  failResolveIO x

resolve :: (GQLQuery a , GQLMutation b) => GQLRoot a b -> GQLRequest -> ResolveIO JSType
resolve rootResolver body = do
    let queryRoot = queryResolver rootResolver
    let mutationRoot = mutationResolver rootResolver
    rootGQL  <- ExceptT $ pure $ parseGQL body
    queryBody <- validate (schema queryRoot mutationRoot) rootGQL
    case  queryBody of
      QueryOperator name query -> encodeQuery queryRoot (schema queryRoot mutationRoot) query
      MutationOperator name mutation -> encodeMutation mutationRoot (schema queryRoot mutationRoot) mutation

interpreter :: (GQLQuery a , GQLMutation b)=> GQLRoot a b -> GQLRequest -> IO GQLResponse
interpreter rootResolver request = do
  value <- runExceptT $ resolve rootResolver request
  case value of
    Left x -> pure $ Errors x
    Right x -> pure$ Data x

eitherToResponse :: (a -> a) -> Either String a -> ResolveIO a
eitherToResponse f (Left  x) = failResolveIO $ errorMessage $ pack x
eitherToResponse f (Right x) = pure (f x)
