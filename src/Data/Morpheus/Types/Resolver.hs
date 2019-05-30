{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Types.Resolver
  ( (::->)
  , (::->>)
  , Resolver(..)
  , Result(..)
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data QUERY

data MUTATION

type family RESOLVER a b

{-
  Monad of Query Resolver
-}
type a ::-> b = Resolver QUERY a b

type instance RESOLVER QUERY b = Either String b

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
        Left x -> return $ Left x
        Right y {--}
         -> do
          let (Resolver x) = func2 y
          x args

{-
  Monad of Mutation and Subscription Resolver
-}
type a ::->> b = Resolver MUTATION a b

type instance RESOLVER MUTATION b = Either String (Result b)

newtype Resolver t a b =
  Resolver (a -> IO (RESOLVER t b))
  deriving (Generic)

data Result a = Result
  { resultValue   :: a
  , resultEffects :: [Text]
  } deriving (Show)

instance Functor Result where
  fmap func (Result value effect) = Result (func value) effect

instance Applicative Result where
  pure value = Result value []
  Result func e1 <*> Result value e2 = Result (func value) (e1 ++ e2)

instance Monad Result where
  return = pure
  (Result v1 e1) >>= func2 = do
    let Result v2 e2 = func2 v1
    Result v2 (e2 ++ e1)

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
        Right (Result v1' e1') -> do
          let (Resolver x') = func2 v1'
          v2 <- x' args
          case v2 of
            Left error'            -> return $ Left error'
            Right (Result v2' e2') -> return $ Right $ Result v2' (e1' ++ e2')
