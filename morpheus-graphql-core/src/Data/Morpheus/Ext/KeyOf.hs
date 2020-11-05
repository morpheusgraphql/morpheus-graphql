{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.KeyOf
  ( KeyOf (..),
    toPair,
  )
where

import Data.Morpheus.Ext.Map
  ( Indexed (..),
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName (..),
    Ref (..),
    TypeName (..),
    TypeNameRef (..),
  )
import Relude

class (Eq k, Hashable k) => KeyOf k a | a -> k where
  keyOf :: a -> k

instance (Eq k, Hashable k) => KeyOf k (k, a) where
  keyOf = fst

instance KeyOf FieldName Ref where
  keyOf = refName

instance KeyOf TypeName TypeNameRef where
  keyOf = typeNameRef

instance (Eq k, Hashable k) => KeyOf k (Indexed k a) where
  keyOf = indexedKey

toPair :: KeyOf k a => a -> (k, a)
toPair x = (keyOf x, x)
