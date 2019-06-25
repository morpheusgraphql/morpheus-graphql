{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Types.Resolver
  ( (::->)
  , (::->>)
  , WithEffect(..)
  , Resolver(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data QUERY (m :: * -> *)

data MUTATION (m :: * -> *) c

data SUBSCRIPTION (m :: * -> *) c

-- | resolver function wrapper, where
--
--  __p__ is a record of GQL Arguments
--
-- __a__ is result
class MonadResolver m a b where
  data Resolver m a b :: *

type Result m a = m (Either String a)

type ResultAction m c a = Result m (WithEffect c a)

--  Monad Resolver:
instance Monad m => MonadResolver (QUERY m) a b where
  data Resolver (QUERY m) a b = Resolver{unpackResolver ::
                                       a -> Result m b}
                                deriving (Generic)

instance Monad m => MonadResolver (MUTATION m c) a b where
  data Resolver (MUTATION m c) a
       b = MutationResolver{unpackMutationResolver ::
                            a -> ResultAction m c b}
             deriving (Generic)

instance Monad m => MonadResolver (SUBSCRIPTION m c) a b where
  data Resolver (SUBSCRIPTION m c) a
       b = SubscriptionResolver{unpackSubscriptionResolver ::
                                a -> c -> ResultAction m c a}
             deriving (Generic)

-- | resolver without effect
type a ::-> b = Resolver (QUERY IO) a b

--a -> IO Either String b
instance Monad m => Functor (Resolver (QUERY m) a) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      return (func <$> value)

instance Monad m => Applicative (Resolver (QUERY m) a) where
  pure = Resolver . const . return . pure
  Resolver func <*> Resolver resolver =
    Resolver $ \args -> do
      func1 <- func args
      value1 <- resolver args
      return (func1 <*> value1)

instance Monad m => Monad (Resolver (QUERY m) a) where
  return = pure
  (Resolver func1) >>= func2 =
    Resolver $ \args -> do
      value1 <- func1 args
      case value1 of
        Left error'  -> return $ Left error'
        Right value' -> (unpackResolver $ func2 value') args

{-
  a ::->> b
-}
-- | resolver with effects,
-- used for communication between mutation and subscription
type a ::->> b = Resolver (MUTATION IO Text) a b

data WithEffect c a = WithEffect
  { resultEffects :: [c]
  , resultValue   :: a
  } deriving (Show, Functor)

instance Applicative (WithEffect c) where
  pure = WithEffect []
  WithEffect effect1 func <*> WithEffect effect2 value = WithEffect (effect1 ++ effect2) (func value)

instance Monad (WithEffect c) where
  return = pure
  (WithEffect e1 v1) >>= func2 = do
    let WithEffect e2 v2 = func2 v1
    WithEffect (e2 ++ e1) v2

instance Monad m => Functor (Resolver (MUTATION m c) a) where
  fmap func (MutationResolver resolver) =
    MutationResolver $ \args -> do
      value <- resolver args
      case value of
        Left error' -> return $ Left error'
        Right res'  -> return $ Right (func <$> res')

instance Monad m => Applicative (Resolver (MUTATION m c) a) where
  pure = MutationResolver . const . return . Right . pure
  MutationResolver func <*> MutationResolver resolver =
    MutationResolver $ \args -> do
      func1 <- func args
      case func1 of
        Left error' -> return $ Left error'
        Right v1 -> do
          v2 <- resolver args
          case v2 of
            Left error' -> return $ Left error'
            Right v2'   -> return $ Right $ v1 <*> v2'

instance Monad m => Monad (Resolver (MUTATION m c) a) where
  return = pure
  (MutationResolver func1) >>= func2 =
    MutationResolver $ \args -> do
      value1 <- func1 args
      case value1 of
        Left error' -> return $ Left error'
        Right (WithEffect e1' v1') -> do
          let (MutationResolver x') = func2 v1'
          v2 <- x' args
          case v2 of
            Left error'                -> return $ Left error'
            Right (WithEffect e2' v2') -> return $ Right $ WithEffect (e1' ++ e2') v2'
