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
  ( BaseR
  , EffectR
  , EffectT(..)
  , Effect(..)
  , Resolver(..)
  , unpackEffect
  , unpackEffect2
  , addEffect
  , withEffect
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..), runExceptT)
import           Data.Text                               (Text)
import           GHC.Generics                            (Generic)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Validation (GQLErrors, ResolveT)

-- | Pure Resolver without effect
type Pure = Either String

-- | Monad IO resolver without GraphQL effect
type BaseR = Resolver IO

-- | Monad Resolver with GraphQL effects, used for communication between mutation and subscription
type EffectR = Resolver (EffectT IO Text)

-- | resolver function wrapper, where
newtype Resolver m a = Resolver
  { unResolver :: m (Pure a)
  } deriving (Generic, Functor)

instance Monad m => Applicative (Resolver m) where
  pure = Resolver . return . pure
  Resolver func <*> Resolver value =
    Resolver $ do
      func1 <- func
      value1 <- value
      return (func1 <*> value1)

instance Monad m => Monad (Resolver m) where
  return = pure
  (Resolver func1) >>= func2 =
    Resolver $ do
      value1 <- func1
      case value1 of
        Left error'  -> return $ Left error'
        Right value' -> unResolver $ func2 value'

-- | used in mutation or subscription resolver , adds effect to normal resolver
withEffect :: Monad m => [c] -> m a -> EffectT m c a
withEffect channels = EffectT . fmap (Effect channels)

addEffect :: Monad m => [c] -> EffectT m c a -> EffectT m c a
addEffect channels EffectT {runEffectT = monadEffect} = EffectT $ insertEffect <$> monadEffect
  where
    insertEffect x = x {resultEffects = channels ++ resultEffects x}

unpackEffect2 :: Monad m => ResolveT (EffectT m Text) v -> ResolveT m ([Text], v)
unpackEffect2 x = ExceptT $ unpackEffect x

unpackEffect :: Monad m => ResolveT (EffectT m Text) v -> m (Either GQLErrors ([Text], v))
unpackEffect resolver = do
  (Effect effects eitherValue) <- runEffectT $ runExceptT resolver
  case eitherValue of
    Left errors -> return $ Left errors
    Right value -> return $ Right (effects, value)

data Effect c v = Effect
  { resultEffects :: [c]
  , resultValue   :: v
  } deriving (Functor)

-- | Monad Transformer that sums all effect Together
newtype EffectT m c v = EffectT
  { runEffectT :: m (Effect c v)
  } deriving (Functor)

instance Monad m => Applicative (EffectT m c) where
  pure = EffectT . return . Effect []
  EffectT app1 <*> EffectT app2 =
    EffectT $ do
      (Effect effect1 func) <- app1
      (Effect effect2 val) <- app2
      return $ Effect (effect1 ++ effect2) (func val)

instance Monad m => Monad (EffectT m c) where
  return = pure
  (EffectT m1) >>= mFunc =
    EffectT $ do
      (Effect e1 v1) <- m1
      (Effect e2 v2) <- runEffectT $ mFunc v1
      return $ Effect (e1 ++ e2) v2
