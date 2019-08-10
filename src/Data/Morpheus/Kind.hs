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
  , INPUT_UNION
  ) where

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

-- | extension for graphQL
data INPUT_UNION

-- | GraphQL Arrays , Resolvers and NonNull fields
data WRAPPER
