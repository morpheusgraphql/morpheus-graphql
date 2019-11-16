{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Data.Morpheus.Types.Internal.Validation
  ( GQLError(..)
  , Position(..)
  , GQLErrors
  , Validation
  , Result(..)
  , Failure(..)
  , ExceptGQL
  , toExceptGQL
  , ResultT(..)
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Semigroup                 ( (<>) )

data GQLError = GQLError
  { message      :: Text
  , locations :: [Position]
  } deriving (Show, Generic, FromJSON, ToJSON)

type GQLErrors = [GQLError]


newtype ResultT event concurency m a = ResultT { runResultT :: m (Result event concurency GQLError a )  }
  deriving (Functor)

instance Applicative m => Applicative (ResultT event concurency m) where
  pure = ResultT . pure . pure
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) app1 app2

instance Monad m => Monad (ResultT event concurency m) where
  return = pure
  (ResultT m1) >>= mFunc = ResultT $ do
    result1 <- m1
    case result1 of
      Failure errors       -> pure $ Failure errors
      Success value1 w1 e1 -> do
        result2 <- runResultT (mFunc value1)
        case result2 of
          Failure errors   -> pure $ Failure (errors <> w1)
          Success v2 w2 e2 -> return $ Success v2 (w1 <> w2) (e1 <> e2)

data Result events (concurency :: Bool) error a =
  Success { result :: a , warnings :: [error] , events:: [events] }
  | Failure [error] deriving (Functor)

instance Applicative (Result e cocnurency  error) where
  pure x = Success x [] []
  Success f w1 e1 <*> Success x w2 e2 = Success (f x) (w1 <> w2) (e1 <> e2)
  Failure e1      <*> Failure e2      = Failure (e1 <> e2)
  Failure e       <*> Success _ w _   = Failure (e <> w)
  Success _ w _   <*> Failure e       = Failure (e <> w)

instance Monad (Result e  cocnurency error)  where
  return = pure
  Success v w1 e1 >>= fm = case fm v of
    (Success x w2 e2) -> Success x (w1 <> w2) (e1 <> e2)
    (Failure e      ) -> Failure (e <> w1)
  Failure e >>= _ = Failure e

type Validation = Result () 'True GQLError

toExceptGQL :: Monad m => Validation a -> ExceptT GQLErrors m a
toExceptGQL Success { result } = pure result
toExceptGQL (Failure e)        = ExceptT $ pure $ Left e

class Applicative f =>  Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure [error] (Result ev con error) where
  failure = Failure

instance Failure error (Either error) where
  failure = Left

type ExceptGQL = ExceptT GQLErrors
