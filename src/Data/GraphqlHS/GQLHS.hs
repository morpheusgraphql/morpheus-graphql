{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.GraphqlHS.GQLHS
    ( interpreter
    , GQLResponce
    , GQLRecord
    , GQLRoot
    , GQLArgs
    , Resolver(resolve)
    , (::->)(Some, None)
    , GQLRequest(..)
    )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Data.GraphqlHS.Generics.GQLRecord
                                                ( GQLRecord )
import           Data.GraphqlHS.Generics.GQLRoot
                                                ( GQLRoot(decode) )
import           Data.GraphqlHS.Generics.GQLArgs
                                                ( GQLArgs )
import           Data.GraphqlHS.Generics.Resolver
                                                ( Resolver(resolve) )
import           Data.GraphqlHS.Parser.Parser   ( parseGQL )
import           Data.GraphqlHS.Types.Types     ( (::->)(Some, None)
                                                , GQLResponce
                                                , GQLRequest(..)
                                                , Eval(..)
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )


interpreter :: GQLRoot a => Proxy a -> a -> GQLRequest -> IO GQLResponce
interpreter schema rootValue x = case (parseGQL . query) x of
    Val  x -> decode rootValue x
    Fail x -> pure (Fail x)

