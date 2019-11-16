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
  , JSONError(..)
  , Validation
  , Computation(..)
  , Failure(..)
  , ExceptGQL
  , toExceptGQL
  )
where

import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data GQLError = GQLError
  { desc      :: Text
  , positions :: [Position]
  } deriving (Show)

type GQLErrors = [GQLError]

data JSONError = JSONError
  { message   :: Text
  , locations :: [Position]
  } deriving (Show, Generic, FromJSON, ToJSON)

{-
newtype CompT m a = CompT { runCompT :: m (Computation 'True GQLError a )  }
  deriving (Functor)

instance Functor m => Applicative (CompT m)

instance Monad m => Monad (CompT m)
-}

data Computation events (concurency :: Bool) error a =
  Success { result :: a , warnings :: [error] , events:: [events] }
  | Failure [error] deriving (Functor)

instance Applicative (Computation e cocnurency  error) where
  pure x = Success x [] []
  Success f w1 e1 <*> Success x w2 e2 = Success (f x) (w1 <> w2) (e1 <> e2)
  Failure e1      <*> Failure e2      = Failure (e1 <> e2)
  Failure e       <*> Success _ w _   = Failure (e <> w)
  Success _ w _   <*> Failure e       = Failure (e <> w)

instance Monad (Computation e  cocnurency error)  where
  return = pure
  Success v w1 e1 >>= fm = case fm v of
    (Success x w2 e2) -> Success x (w1 <> w2) (e1 <> e2)
    (Failure e      ) -> Failure (e <> w1)
  Failure e >>= _ = Failure e

type Validation = Computation () 'True GQLError

toExceptGQL :: Monad m => Validation a -> ExceptT GQLErrors m a
toExceptGQL Success { result } = pure result
toExceptGQL (Failure e)        = ExceptT $ pure $ Left e

class Applicative f =>  Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure [error] (Computation ev con error) where
  failure = Failure

instance Failure error (Either error) where
  failure = Left

type ExceptGQL = ExceptT GQLErrors
