{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Types.Resolver
  ( (::->)
  , Resolver(..)
  ) where

import           GHC.Generics (Generic)

type a ::-> b = Resolver () a b

newtype Resolver c a b =
  Resolver (a -> IO (Either String (b, [c])))
  deriving (Generic)

instance Functor (Resolver () p) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      case value of
        Left error'  -> return $ Left error'
        Right (x, y) -> return $ Right (func x, y)

instance Applicative (Resolver () p) where
  pure = Resolver . const . return . Right . (, [])
  Resolver func <*> Resolver resolver =
    Resolver $ \args -> do
      func1 <- func args
      case func1 of
        Left error' -> return $ Left error'
        Right (func1', context') -> do
          value1 <- resolver args
          return ((, context') . func1' . fst <$> value1)

instance Monad (Resolver () p) where
  return = pure
  (Resolver func1) >>= func2 =
    Resolver $ \args -> do
      value1 <- func1 args
      case value1 of
        Left error' -> return $ Left error'
        Right (x, _) -> do
          let (Resolver x') = func2 x
          x' args
