{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
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
  , Pure
  , Resolver
  , gqlResolver
  , gqlEffectResolver
  , liftEffectResolver
  , unpackEffect
  , unpackEffect2
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..), runExceptT)
import           Data.Text                               (Text)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Validation (GQLErrors, ResolveT)

-- | Pure Resolver without effect
type Pure = Either String

-- | Monad IO resolver without GraphQL effect
type BaseR = Resolver IO

-- | Monad Resolver with GraphQL effects, used for communication between mutation and subscription
type EffectR = Resolver (EffectT IO Text)

-- | Resolver Monad Transformer
type Resolver = ExceptT String

-- | GraphQL Resolver
gqlResolver :: m (Either String a) -> Resolver m a
gqlResolver = ExceptT

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
gqlEffectResolver :: Monad m => [c] -> (EffectT m c) (Either String a) -> Resolver (EffectT m c) a
gqlEffectResolver channels = ExceptT . insertEffect channels

insertEffect :: Monad m => [c] -> EffectT m c a -> EffectT m c a
insertEffect channels EffectT {runEffectT = monadEffect} = EffectT $ effectPlus <$> monadEffect
  where
    effectPlus x = x {resultEffects = channels ++ resultEffects x}

-- | lift Normal resolver inside Effect Resolver
liftEffectResolver :: Monad m => [c] -> m (Either String a) -> Resolver (EffectT m c) a
liftEffectResolver channels = ExceptT . EffectT . fmap (Effect channels)

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
