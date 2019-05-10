{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Morpheus.Generics.UnionRep
  ( UnionRep(..)
  ) where

import           Data.Morpheus.Schema.Internal.Types (Field, TypeLib)
import           Data.Proxy                          (Proxy (..))
import           GHC.Generics

class UnionRep f where
  possibleTypes :: Proxy f -> [(Field, TypeLib -> TypeLib)]

instance UnionRep f => UnionRep (M1 D x f) where
  possibleTypes _ = possibleTypes (Proxy @f)

instance UnionRep f => UnionRep (M1 C x f) where
  possibleTypes _ = possibleTypes (Proxy @f)

instance UnionRep U1 where
  possibleTypes _ = []

instance (UnionRep a, UnionRep b) => UnionRep (a :+: b) where
  possibleTypes _ = possibleTypes (Proxy @a) ++ possibleTypes (Proxy @b)
