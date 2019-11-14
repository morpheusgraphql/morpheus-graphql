{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Data.Morpheus.Types.Internal.Validation
  ( GQLError(..)
  , Position(..)
  , GQLErrors
  , JSONError(..)
  , Validation(..)
  , GQLCatch(..)
  , Failure(..)
  )
where

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

data Validation a = Success {
  validationValue :: a,
  validationWarnings :: [GQLError]
  } |
  Failure {
    validationErrors :: [GQLError]
  } deriving (Functor)

instance Applicative Validation where
  pure x = Success x []
  Success f w1 <*> Success x w2 = Success (f x) (w1 <> w2)
  Failure e1   <*> Failure e2   = Failure (e1 <> e2)
  Failure e    <*> Success _ w  = Failure (e <> w)
  Success _ w  <*> Failure e    = Failure (e <> w)

instance Monad Validation where
  return = pure
  Success v w1 >>= fm = case fm v of
    (Success x w2) -> Success x (w1 <> w2)
    (Failure e   ) -> Failure e
  Failure e >>= _ = Failure e


class Applicative f =>  Failure error (f :: * -> *) where
  failure :: error -> f v
  toEither :: f v -> Either error v
  fromEither :: Either error v -> f v

instance Failure [GQLError] Validation where
  failure = Failure
  toEither (Success value _) = Right value
  toEither (Failure errors ) = Left errors
  fromEither (Left  errors) = Failure errors
  fromEither (Right value ) = Success value []


class GQLCatch f where
  catch :: (GQLError -> f v) -> f v -> f v
