{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus
    ( interpreter
    , GQLResponce
    , GQLSelection
    , GQLRoot
    , GQLArgs
    , (::->)(..)
    , GQLRequest(..)
    , eitherToResponce
    , EvalIO(..)
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
                                                , GQLResponce
                                                , GQLRequest(..)
                                                , Eval(..)
                                                , EvalIO(..)
                                                , failEvalIO
                                                , JSType
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.Morpheus.ErrorMessage    ( errorMessage )
import           Control.Monad.Trans.Except     ( runExceptT
                                                , ExceptT(..)
                                                )


resolve :: GQLRoot a => EvalIO a -> GQLRequest -> EvalIO JSType
resolve rootValue body = do
    root <- rootValue
    gql  <- ExceptT $ pure $ parseGQL body
    encode root gql

interpreter :: GQLRoot a => EvalIO a -> GQLRequest -> IO GQLResponce
interpreter root request = runExceptT $ resolve root request

eitherToResponce :: (a -> a) -> Either String a -> EvalIO a
eitherToResponce f (Left  x) = failEvalIO $ errorMessage $ pack $ x
eitherToResponce f (Right x) = pure (f x)
