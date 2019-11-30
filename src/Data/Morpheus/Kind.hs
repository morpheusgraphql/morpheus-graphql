{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
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
  , GQL_KIND
  , Context(..)
  , VContext(..)
  , ResContext(..)
  , AUTO
  , INPUT
  )
where

import           Data.Morpheus.Types.Internal.AST
                                                ( OperationType(..) )

data GQL_KIND
  = SCALAR
  | OBJECT
  | ENUM
  | INPUT_OBJECT
  | UNION
  | INPUT
  | WRAPPER
  | AUTO


data ResContext (kind :: GQL_KIND) (operation:: OperationType) event (m :: * -> * )  value = ResContext

--type ObjectConstraint a =
-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data Context (kind :: GQL_KIND) a =
  Context

newtype VContext (kind :: GQL_KIND) a = VContext
  { unVContext :: a
  }

-- | GraphQL Scalar: Int, Float, String, Boolean or any user defined custom Scalar type
type SCALAR = 'SCALAR

-- | GraphQL Object
type OBJECT = 'OBJECT

-- | GraphQL Enum
type ENUM = 'ENUM

-- | GraphQL input Object
type INPUT_OBJECT = 'INPUT_OBJECT

-- | GraphQL Union
type UNION = 'UNION

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'WRAPPER

type AUTO = 'AUTO

type INPUT = 'INPUT
