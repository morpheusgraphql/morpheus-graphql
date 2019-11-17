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
  , fromEither
  , toEither
  , mapExceptGQL
  , fromEitherSingle
  , mapUnitToEvents
  , getResultEvents
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position(..) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Semigroup                 ( (<>) )


class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

data GQLError = GQLError
  { message      :: Text
  , locations :: [Position]
  } deriving (Show, Generic, FromJSON, ToJSON)

type GQLErrors = [GQLError]
type ExceptGQL = ExceptT GQLErrors
type Validation = Result () 'True GQLError

--
-- Result
--
--
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

instance Failure [error] (Result ev con error) where
  failure = Failure


getResultEvents :: Result event c e a -> [event]
getResultEvents Success { events } = events
getResultEvents _                  = []


toExceptGQL :: Monad m => Validation a -> ExceptT GQLErrors m a
toExceptGQL = ExceptT . pure . toEither

toEither :: Result ev co er a -> Either [er] a
toEither (Failure e)        = Left e
toEither Success { result } = Right result

fromEither :: Either [er] a -> Result ev co er a
fromEither (Left  e) = Failure e
fromEither (Right a) = Success a [] []

fromEitherSingle :: Either er a -> Result ev co er a
fromEitherSingle (Left  e) = Failure [e]
fromEitherSingle (Right a) = Success a [] []

mapExceptGQL :: Functor m => ResultT e error con m a -> ExceptT [error] m a
mapExceptGQL (ResultT x) = ExceptT $ toEither <$> x

-- ResultT
newtype ResultT event error (concurency :: Bool)  (m :: * -> * ) a = ResultT { runResultT :: m (Result event concurency error a )  }
  deriving (Functor)

instance Applicative m => Applicative (ResultT event error concurency m) where
  pure = ResultT . pure . pure
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) app1 app2

instance Monad m => Monad (ResultT event error concurency m) where
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


mapUnitToEvents
  :: Functor m
  => ResultT () error concurency m a
  -> ResultT event error concurency m a
mapUnitToEvents resT = ResultT $ replace <$> runResultT resT
 where
  replace (Success v w _) = Success v w []
  replace (Failure e    ) = Failure e


instance Applicative m => Failure String (ResultT ev GQLError con m) where
  failure x =
    ResultT $ pure $ Failure [GQLError { message = pack x, locations = [] }]

