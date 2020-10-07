{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.Failure
  ( Failure (..),
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
  )
import Prelude
  ( (.),
    Either (..),
  )

-- Failure: for custom Morpheus GrapHQL errors
class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Monad m, Failure errors m) => Failure errors (ReaderT ctx m) where
  failure = lift . failure
