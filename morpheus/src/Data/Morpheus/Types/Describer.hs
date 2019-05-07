{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Types.Describer
  ( (::->)(..)
  ) where

import           GHC.Generics (Generic)

newtype a ::-> b =
  Resolver (a -> IO (Either String b))
  deriving (Generic)

instance Functor ((::->) p) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      return (func <$> value)

instance Applicative ((::->) p) where
  pure = Resolver . const . return . pure
  Resolver func <*> Resolver resolver =
    Resolver $ \args -> do
      func1 <- func args
      value1 <- resolver args
      return (func1 <*> value1)

instance Monad ((::->) p) where
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
