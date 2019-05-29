{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Types.Resolver
  ( (::->)
  , Resolver(..)
  , Result(..)
  , GQLResult
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Result e a = Result
  { result  :: a
  , effects :: [e]
  } deriving (Show)

instance Functor (Result e) where
  fmap func (Result value effect) = Result (func value) effect

instance Applicative (Result e) where
  pure value = Result value []
  Result func e1 <*> Result value e2 = Result (func value) (e1 ++ e2)

instance Monad (Result e) where
  return = pure
  (Result v1 e1) >>= func2 = do
    let Result v2 e2 = func2 v1
    Result v2 (e2 ++ e1)

type GQLResult a = Result Text a

type a ::-> b = Resolver Text a b

newtype Resolver m a b =
  Resolver (a -> IO (Either String (Result m b)))
  deriving (Generic)

instance Functor (Resolver m p) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      case value of
        Left error' -> return $ Left error'
        Right res'  -> return $ Right (func <$> res')

instance Applicative (Resolver m p) where
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

instance Monad (Resolver m p) where
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
