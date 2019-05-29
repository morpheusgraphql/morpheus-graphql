{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Data.Morpheus.Kind.Internal
  ( SCALAR
  , OBJECT
  , ENUM
  , WRAPPER
  , UNION
  , INPUT_OBJECT
  , KIND
  , GQLConstraint
  ) where

import Data.Morpheus.Types.Resolver
import Data.Text (Text)
import GHC.Exts (Constraint)

-- Hidden KIND type System API
type family KIND a :: *

-- default Type Instances
type instance KIND Text = SCALAR

type instance KIND Int = SCALAR

type instance KIND Float = SCALAR

type instance KIND Bool = SCALAR

type instance KIND (Maybe a) = WRAPPER

type instance KIND [a] = WRAPPER

type instance KIND (Resolver c p a) = WRAPPER

-- default Data Kinds
data SCALAR

data OBJECT

data ENUM

data INPUT_OBJECT

data WRAPPER

data UNION

--data LIST
type family GQLConstraint a b :: Constraint