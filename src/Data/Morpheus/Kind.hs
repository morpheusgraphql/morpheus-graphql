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
  , OUTPUT
  , INPUT
  )
where

import           Data.Morpheus.Types.Internal.AST
                                                ( OperationType(..) )

data GQL_KIND
  = SCALAR
  | ENUM
  | INPUT
  | OUTPUT
  | WRAPPER
  -- DEPRECATED CONS
  | UNION
  | INPUT_OBJECT
  | OBJECT


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

-- | GraphQL Enum
type ENUM = 'ENUM

-- | GraphQL Arrays , Resolvers and NonNull fields
type WRAPPER = 'WRAPPER

-- | GraphQL Object and union
type OUTPUT = 'OUTPUT

-- | GraphQL input Object and input union
type INPUT = 'INPUT

{-# DEPRECATED INPUT_OBJECT "replaced with more generalised kind: INPUT" #-}
-- | GraphQL input Object
type INPUT_OBJECT = 'INPUT_OBJECT

{-# DEPRECATED UNION "use: deriving(GQLType), will be automatically inferred" #-}
-- | GraphQL Union
type UNION = 'UNION

{-# DEPRECATED OBJECT "use: deriving(GQLType), will be automatically inferred" #-}
--{-# WARNING unsafePerformIO "This is unsafe; I hope you know what you're doing" #-}
-- | GraphQL Object
type OBJECT = 'OBJECT