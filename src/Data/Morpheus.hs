{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus
    ( interpreter
    , GQLResponse
    , GQLSelection
    , GQLRoot
    , GQLArgs
    , (::->)(..)
    , GQLRequest(..)
    , eitherToResponse
    , EvalIO(..)
    , GQLInput
    , GQLEnum(unpackEnum)
    , GQLEnumType
    )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Morpheus.Generics.GQLSelection
                                                ( GQLSelection )
import           Data.Morpheus.Generics.GQLRoot
                                                ( GQLRoot(encode) )
import           Data.Morpheus.Generics.GQLArgs
                                                ( GQLArgs )
import           Data.Morpheus.Parser.Parser   ( parseGQL )
import           Data.Morpheus.Types.Types     ( (::->)(Resolver)
                                                , GQLResponse(..)
                                                , GQLRequest(..)
                                                , Eval(..)
                                                , EvalIO(..)
                                                , failEvalIO
                                                , JSType
                                                , GQLEnum(unpackEnum)
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.Morpheus.ErrorMessage    ( errorMessage )
import           Control.Monad.Trans.Except     ( runExceptT
                                                , ExceptT(..)
                                                )
import          Data.Morpheus.Generics.GQLInput (GQLInput)
import          Data.Morpheus.Generics.GQLEnumType(GQLEnumType)


resolve :: GQLRoot a => EvalIO a -> GQLRequest -> EvalIO JSType
resolve rootValue body = do
    root <- rootValue
    gql  <- ExceptT $ pure $ parseGQL body
    encode root gql

interpreter :: GQLRoot a => EvalIO a -> GQLRequest -> IO GQLResponse
interpreter root request = do
  value <- runExceptT $ resolve root request
  case value of
    Left x -> pure $ Errors x
    Right x -> pure$ Data x

eitherToResponse :: (a -> a) -> Either String a -> EvalIO a
eitherToResponse f (Left  x) = failEvalIO $ errorMessage $ pack x
eitherToResponse f (Right x) = pure (f x)
