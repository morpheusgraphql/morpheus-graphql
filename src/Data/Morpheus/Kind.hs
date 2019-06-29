{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | associating types to GraphQL Kinds
module Data.Morpheus.Kind
  ( SCALAR
  , OBJECT
  , ENUM
  , WRAPPER
  , UNION
  , INPUT_OBJECT
  , KIND
  ) where

import           Data.Map                     (Map)
import           Data.Set                     (Set)
import           Data.Text                    (Text)

-- MORPHEUS
import           Data.Morpheus.Types.Resolver (Resolver)

-- | Type Family to associate type to GraphQL Kind
type family KIND a :: *

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
data SCALAR

-- | GraphQL Object
data OBJECT

-- | GraphQL Enum
data ENUM

-- | GraphQL input Object
data INPUT_OBJECT

-- | GraphQL Union
data UNION

-- | GraphQL Arrays , Resolvers and NonNull fields
data WRAPPER

-- default Type Instances
type instance KIND Text = SCALAR

type instance KIND Int = SCALAR

type instance KIND Float = SCALAR

type instance KIND Bool = SCALAR

type instance KIND (Maybe a) = WRAPPER

type instance KIND [a] = WRAPPER

type instance KIND (a, b) = WRAPPER

type instance KIND (Set a) = WRAPPER

type instance KIND (Map k v) = WRAPPER

type instance KIND (Resolver m a) = WRAPPER

type instance KIND (a -> b) = WRAPPER
