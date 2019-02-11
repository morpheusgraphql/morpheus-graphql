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
                                                , failEvalIO
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.GraphqlHS.ErrorMessage    ( errorMessage )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )


interpreter
    :: GQLRoot a => Proxy a -> IO (Eval a) -> GQLRequest -> IO GQLResponce
interpreter schema rootValue requestBody = do
    root <- rootValue
    case (parseGQL requestBody, root) of
        (Left  x, _      ) -> pure (Left x)
        (Right _, Left x ) -> pure (Left x)
        (Right g, Right r) -> runExceptT (decode r g)

liftIO :: IO a -> EvalIO a
liftIO = lift

eitherToResponce :: (a -> a) -> Either String a -> EvalIO a
eitherToResponce f (Left  x) = failEvalIO $ errorMessage $ pack $ x
eitherToResponce f (Right x) = pure (f x)
