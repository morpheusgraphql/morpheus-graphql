{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
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
  , unpackEffect
  , unpackEffect2
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..), runExceptT)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors, ResolveT)
import           Data.Text                               (Text)
import           GHC.Generics                            (Generic)

-- | resolver function wrapper, where
--
--  __p__ is a record of GQL Arguments
--
-- __a__ is result
newtype Resolver m a b = Resolver
  { unpackResolver :: a -> m (Either String b)
  } deriving (Generic)

-- | resolver without effect
type a ::-> b = Resolver IO a b

--a -> IO Either String b
instance Monad m => Functor (Resolver m a) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      return (func <$> value)

instance Monad m => Applicative (Resolver m a) where
  pure = Resolver . const . return . pure
  Resolver func <*> Resolver resolver =
    Resolver $ \args -> do
      func1 <- func args
      value1 <- resolver args
      return (func1 <*> value1)

instance Monad m => Monad (Resolver m a) where
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
type a ::->> b = Resolver (WithEffect IO Text) a b

unpackEffect2 :: Monad m => ResolveT (WithEffect m Text) v -> ResolveT m ([Text], v)
unpackEffect2 x = ExceptT $ unpackEffect x


unpackEffect :: Monad m => ResolveT (WithEffect m Text) v -> m (Either GQLErrors ([Text], v))
unpackEffect resolver = do
  let WithEffect e v = runExceptT resolver
  effects <- e
  value <- v
  case value of
    Left errors -> return $ Left errors
    Right val   -> return $ Right (effects, val)

data WithEffect m c v = WithEffect
  { resultEffects :: m [c]
  , resultValue   :: m v
  } deriving (Functor)

instance Monad m => Applicative (WithEffect m c) where
  pure = WithEffect (return []) . return
  WithEffect effect1 func <*> WithEffect effect2 value =
    WithEffect
      (do e1 <- effect1
          e2 <- effect2
          return (e1 ++ e2))
      (do x <- value
          f <- func
          return (f x))

instance Monad m => Monad (WithEffect m c) where
  return = pure
  (WithEffect e1 v1) >>= func2 =
    WithEffect
      (do v2 <- v1
          e1' <- e1
          e2 <- resultEffects $ func2 v2
          return (e1' ++ e2))
      (do v2 <- v1
          resultValue $ func2 v2)
