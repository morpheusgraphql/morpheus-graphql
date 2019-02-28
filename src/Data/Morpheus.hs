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
    )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Morpheus.Generics.GQLSelection
                                                ( GQLSelection )
import           Data.Morpheus.Generics.GQLQuery
                                                ( GQLQuery(encode) )
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
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.Morpheus.ErrorMessage    ( errorMessage )
import           Control.Monad.Trans.Except     ( runExceptT
                                                , ExceptT(..)
                                                )
import          Data.Morpheus.Generics.GQLInput (GQLInput)
import          Data.Morpheus.Generics.GQLEnum  (GQLEnum)


resolve :: GQLQuery a => ResolveIO a -> GQLRequest -> ResolveIO JSType
resolve rootResolver body = do
    root <- rootResolver
    query  <- ExceptT $ pure $ parseGQL body
    encode root query

interpreter :: GQLQuery a => ResolveIO a -> GQLRequest -> IO GQLResponse
interpreter rootResolver request = do
  value <- runExceptT $ resolve rootResolver request
  case value of
    Left x -> pure $ Errors x
    Right x -> pure$ Data x

eitherToResponse :: (a -> a) -> Either String a -> ResolveIO a
eitherToResponse f (Left  x) = failResolveIO $ errorMessage $ pack x
eitherToResponse f (Right x) = pure (f x)
