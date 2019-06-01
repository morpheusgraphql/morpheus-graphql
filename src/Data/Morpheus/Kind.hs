{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Kind
  ( SCALAR
  , OBJECT
  , ENUM
  , WRAPPER
  , UNION
  , INPUT_OBJECT
  , KIND
  ) where

import           Data.Morpheus.Types.Resolver (Resolver)
import           Data.Text                    (Text)

type family KIND a :: *

-- default Data Kinds
data SCALAR

data OBJECT

data ENUM

data INPUT_OBJECT

data WRAPPER

data UNION

-- default Type Instances
type instance KIND Text = SCALAR

type instance KIND Int = SCALAR

type instance KIND Float = SCALAR

type instance KIND Bool = SCALAR

type instance KIND (Maybe a) = WRAPPER

type instance KIND [a] = WRAPPER

type instance KIND (Resolver c p a) = WRAPPER
