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
    , EvalIO(..)
    , liftIO
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
                                                , EvalIO(..)
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.GraphqlHS.ErrorMessage    ( errorMessage )


interpreter
    :: GQLRoot a => Proxy a -> IO (Eval a) -> GQLRequest -> IO GQLResponce
interpreter schema rootValue x = do
    rv <- rootValue
    case rv of
        Val rvx -> case (parseGQL . query) x of
            Val x -> case decode rvx x of
                IOVal  x -> x >>= pure . Val
                IOFail x -> pure (Fail x)
            Fail x -> pure (Fail x)
        Fail x -> pure (Fail x)


liftIO :: IO a -> EvalIO a
liftIO = IOVal

eitherToResponce :: (a -> a) -> Either String a -> EvalIO a
eitherToResponce f (Left  x) = IOFail $ errorMessage $ pack $ x
eitherToResponce f (Right x) = pure (f x)
