{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.SemigroupM
  ( SemigroupM (..),
    (<:>),
    concatTraverse,
    join,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import Data.Morpheus.Error.NameCollision (NameCollision)
import Data.Morpheus.Ext.Empty (Empty (..))
import Data.Morpheus.Ext.KeyOf (KeyOf (..))
import Data.Morpheus.Ext.Map
  ( fromListT,
    runResolutionT,
  )
import Data.Morpheus.Internal.Utils
  ( Elems (..),
    Failure,
    failOnDuplicates,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    Ref,
    ValidationErrors,
  )
import Relude
  ( ($),
    Applicative (..),
    Monad (..),
    Semigroup (..),
    Traversable (..),
  )

class SemigroupM (m :: * -> *) a where
  mergeM :: [Ref FieldName] -> a -> a -> m a

instance
  ( NameCollision a,
    Monad m,
    KeyOf k a,
    Failure ValidationErrors m
  ) =>
  SemigroupM m (HashMap k a)
  where
  mergeM _ x y = runResolutionT (fromListT $ HM.toList x <> HM.toList y) HM.fromList failOnDuplicates

concatTraverse ::
  ( Monad m,
    Failure ValidationErrors m,
    Elems a ca,
    SemigroupM m cb,
    Empty cb
  ) =>
  (a -> m cb) ->
  ca ->
  m cb
concatTraverse f smap =
  traverse f (elems smap)
    >>= join

join ::
  ( Empty a,
    Monad m,
    Failure ValidationErrors m,
    SemigroupM m a
  ) =>
  [a] ->
  m a
join = __join empty
  where
    __join acc [] = pure acc
    __join acc (x : xs) = acc <:> x >>= (`__join` xs)

(<:>) :: SemigroupM m a => a -> a -> m a
(<:>) = mergeM []
