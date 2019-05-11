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

import           Data.Morpheus.Schema.Internal.AST (Field, TypeLib)
import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text)
import           GHC.Generics

class UnionRep f where
  possibleTypes :: Proxy f -> [(Field, TypeLib -> TypeLib)]
  currentType :: f a -> (Text, ())

instance UnionRep f => UnionRep (M1 D x f) where
  possibleTypes _ = possibleTypes (Proxy @f)
  currentType (M1 x) = currentType x

instance UnionRep f => UnionRep (M1 C x f) where
  possibleTypes _ = possibleTypes (Proxy @f)
  currentType (M1 x) = currentType x

instance (UnionRep a, UnionRep b) => UnionRep (a :+: b) where
  possibleTypes _ = possibleTypes (Proxy @a) ++ possibleTypes (Proxy @b)
  currentType (L1 x) = currentType x
  currentType (R1 x) = currentType x
