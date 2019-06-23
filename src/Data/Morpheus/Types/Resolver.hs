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
  , MonadResolver(..)
  , (:->)(..)
  , (:~>)(..)
  , WithEffect(..)
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | resolver function wrapper, where
--
--  __p__ is a record of GQL Arguments
--
-- __a__ is result
class MonadResolver args t where
  data args :-> t :: *
  data args :~> t :: *

instance Monad m => MonadResolver (m args) t where
  data m args :-> t = Resolver{unpackResolver ::
                             args -> m (Either String t)}
                      deriving (Generic)
  data m args :~> t = ActionResolver{unpackActionResolver ::
                                   args -> m (Either String (WithEffect t))}
                      deriving (Generic)
  --Resolver (args -> m (Either String t))

--instance Monad m => MResolver MUTATION (m (WithEffect t)) where
--  data args :-> m (WithEffect t) = ActionResolver { unpackActionResolver :: args -> m (WithEffect t) } deriving (Generic)
{-
  Monad of Query Resolver:
  a ::-> b  : returns pure values without any effect
-}
-- | resolver without effect
type a ::-> b = IO a :-> b

--a -> IO Either String b
instance Monad m => Functor ((:->) (m p)) where
  fmap func (Resolver resolver) =
    Resolver $ \args -> do
      value <- resolver args
      return (func <$> value)

instance Monad m => Applicative ((:->) (m p)) where
  pure = Resolver . const . return . pure
  Resolver func <*> Resolver resolver =
    Resolver $ \args -> do
      func1 <- func args
      value1 <- resolver args
      return (func1 <*> value1)

instance Monad m => Monad ((:->) (m p)) where
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
type a ::->> b = IO a :~> WithEffect b

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

instance Monad m => Functor ((:~>) (m p)) where
  fmap func (ActionResolver resolver) =
    ActionResolver $ \args -> do
      value <- resolver args
      case value of
        Left error' -> return $ Left error'
        Right res'  -> return $ Right (func <$> res')

instance Monad m => Applicative ((:~>) (m p)) where
  pure = ActionResolver . const . return . Right . pure
  ActionResolver func <*> ActionResolver resolver =
    ActionResolver $ \args -> do
      func1 <- func args
      case func1 of
        Left error' -> return $ Left error'
        Right v1 -> do
          v2 <- resolver args
          case v2 of
            Left error' -> return $ Left error'
            Right v2'   -> return $ Right $ v1 <*> v2'

instance Monad m => Monad ((:~>) (m p)) where
  return = pure
  (ActionResolver func1) >>= func2 =
    ActionResolver $ \args -> do
      value1 <- func1 args
      case value1 of
        Left error' -> return $ Left error'
        Right (WithEffect e1' v1') -> do
          let (ActionResolver x') = func2 v1'
          v2 <- x' args
          case v2 of
            Left error'                -> return $ Left error'
            Right (WithEffect e2' v2') -> return $ Right $ WithEffect (e1' ++ e2') v2'
