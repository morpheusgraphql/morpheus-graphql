{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Types.Resolver
  ( (::->)
  , (::->>)
  , Resolver(..)
  , WithEffect(..)
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data QUERY

data MUTATION

type family RESOLVER a b

newtype Resolver t a b = Resolver
  { unpackResolver :: a -> IO (RESOLVER t b)
  } deriving (Generic)

{-
  Monad of Query Resolver:
  a ::-> b  : returns pure values without any effect
-}
type instance RESOLVER QUERY b = Either String b

type a ::-> b = Resolver QUERY a b

instance Functor (Resolver QUERY a) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      return (func <$> value)

instance Applicative (Resolver QUERY a) where
  pure = Resolver . const . return . pure
  Resolver func <*> Resolver resolver =
    Resolver $ \args -> do
      func1 <- func args
      value1 <- resolver args
      return (func1 <*> value1)

instance Monad (Resolver QUERY a) where
  return = pure
  (Resolver func1) >>= func2 =
    Resolver $ \args -> do
      value1 <- func1 args
      case value1 of
        Left error'  -> return $ Left error'
        Right value' -> (unpackResolver $ func2 value') args

{-
  a ::->> b : Resolver with effects: [ChanelID]
  Monad of Mutation and Subscription Resolver
-}
type a ::->> b = Resolver MUTATION a b

type instance RESOLVER MUTATION b = Either String (WithEffect b)

data WithEffect a = WithEffect
  { resultEffects :: [Text]
  , resultValue   :: a
  } deriving (Show, Functor)

instance Applicative WithEffect where
  pure = WithEffect []
  WithEffect effect1 func <*> WithEffect effect2 value = WithEffect (effect1 ++ effect2) (func value)

instance Monad WithEffect where
  return = pure
  (WithEffect e1 v1) >>= func2 = do
    let WithEffect e2 v2 = func2 v1
    WithEffect (e2 ++ e1) v2

instance Functor (Resolver MUTATION p) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      case value of
        Left error' -> return $ Left error'
        Right res'  -> return $ Right (func <$> res')

instance Applicative (Resolver MUTATION p) where
  pure = Resolver . const . return . Right . pure
  Resolver func <*> Resolver resolver =
    Resolver $ \args -> do
      func1 <- func args
      case func1 of
        Left error' -> return $ Left error'
        Right v1 -> do
          v2 <- resolver args
          case v2 of
            Left error' -> return $ Left error'
            Right v2'   -> return $ Right $ v1 <*> v2'

instance Monad (Resolver MUTATION p) where
  return = pure
  (Resolver func1) >>= func2 =
    Resolver $ \args -> do
      value1 <- func1 args
      case value1 of
        Left error' -> return $ Left error'
        Right (WithEffect e1' v1') -> do
          let (Resolver x') = func2 v1'
          v2 <- x' args
          case v2 of
            Left error'                -> return $ Left error'
            Right (WithEffect e2' v2') -> return $ Right $ WithEffect (e1' ++ e2') v2'
