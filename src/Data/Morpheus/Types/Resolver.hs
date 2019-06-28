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
  , (::->)
  , (::->>)
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

--type a -> b = Identity (a -> b)
-- | IO resolver without effect
type BaseR = Resolver IO

-- | inline version of IO Resolver
type a ::-> b = BaseR a b

-- | resolver with effects, used for communication between mutation and subscription
type EffectR = Resolver (EffectT IO Text)

-- | inline version of Resolver with effects
type a ::->> b = EffectR a b

-- | resolver function wrapper, where
--
--  __p__ is a record of GQL Arguments
--
-- __a__ is result
newtype Resolver m a b = Resolver
  { unpackResolver :: a -> m (Either String b)
  } deriving (Generic)

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
