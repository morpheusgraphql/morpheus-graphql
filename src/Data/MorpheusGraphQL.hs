{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.MorpheusGraphQL
    ( interpreter
    , GQLResponce
    , GQLRecord
    , GQLRoot
    , GQLArgs
    , (::->)(..)
    , GQLRequest(..)
    , eitherToResponce
    , Eval
    )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.GraphqlHS.Generics.GQLRecord
                                                ( GQLRecord )
import           Data.GraphqlHS.Generics.GQLRoot
                                                ( GQLRoot(decode) )
import           Data.GraphqlHS.Generics.GQLArgs
                                                ( GQLArgs )
import           Data.GraphqlHS.Parser.Parser   ( parseGQL )
import           Data.GraphqlHS.Types.Types     ( (::->)(Resolver)
                                                , GQLResponce
                                                , GQLRequest(..)
                                                , Eval(..)
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.GraphqlHS.ErrorMessage    ( handleError )


interpreter
    :: GQLRoot a => Proxy a -> IO (Eval a) -> GQLRequest -> IO GQLResponce
interpreter schema rootValue x = do
    rv <- rootValue
    case rv of
        Val rvx -> case (parseGQL . query) x of
            Val  x -> decode rvx x
            Fail x -> pure (Fail x)
        Fail x -> pure (Fail x)

eitherToResponce :: (a -> a) -> Either String a -> IO (Eval a)
eitherToResponce f (Left  x) = pure $ handleError $ pack $ x
eitherToResponce f (Right x) = pure $ pure (f x)
