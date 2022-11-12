{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveWith (..),
    DeriveValueOptions (..),
    DeriveTypeOptions (..),
    deriveValue,
    deriveTypeWith,
  )
where

import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( conNameProxy,
    isRecordProxy,
    selNameProxy,
  )
import Data.Morpheus.Server.Deriving.Utils.Types
import Data.Morpheus.Server.Types.Internal
  ( TypeData (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeName,
    TypeRef (..),
  )
import GHC.Generics
  ( C,
    Constructor,
    D,
    Datatype,
    Generic (..),
    K1 (..),
    M1 (..),
    Meta,
    Rec0,
    S,
    Selector,
    U1 (..),
    (:*:) (..),
    (:+:) (..),
  )
import Relude hiding (undefined)

data DeriveValueOptions kind gql c v = DeriveValueOptions
  { __valueTypeName :: TypeName,
    __valueApply :: forall a. c a => a -> v,
    __valueGetType :: forall f a. gql a => f a -> TypeData
  }

data DeriveTypeOptions kind gql derive v = DeriveTypeOptions
  { __typeApply :: forall f a. derive a => f a -> v,
    __typeGetType :: forall f a. gql a => f a -> TypeData
  }

deriveValue ::
  (Generic a, DeriveWith gql constraint value (Rep a)) =>
  DeriveValueOptions kind gql constraint value ->
  a ->
  DataType value
deriveValue options = deriveTypeValue options . from

deriveTypeWith ::
  forall kind gql c v kinded a.
  (DeriveWith gql c v (Rep a)) =>
  DeriveTypeOptions kind gql c v ->
  kinded kind a ->
  [ConsRep v]
deriveTypeWith options _ = deriveTypeDefinition options (Proxy @(Rep a))

--  GENERIC UNION
class DeriveWith (gql :: Type -> Constraint) (c :: Type -> Constraint) (v :: Type) f where
  deriveTypeValue :: DeriveValueOptions kind gql c v -> f a -> DataType v
  deriveTypeDefinition :: DeriveTypeOptions kind gql c v -> proxy f -> [ConsRep v]

instance (Datatype d, DeriveWith gql c v f) => DeriveWith gql c v (M1 D d f) where
  deriveTypeValue options (M1 src) = (deriveTypeValue options src) {dataTypeName = __valueTypeName options}
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (DeriveWith gql c v a, DeriveWith gql c v b) => DeriveWith gql c v (a :+: b) where
  deriveTypeValue f (L1 x) = (deriveTypeValue f x) {tyIsUnion = True}
  deriveTypeValue f (R1 x) = (deriveTypeValue f x) {tyIsUnion = True}
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @a) <> deriveTypeDefinition options (Proxy @b)

instance (DeriveFieldRep gql con v f, Constructor c) => DeriveWith gql con v (M1 C c f) where
  deriveTypeValue options (M1 src) =
    DataType
      { dataTypeName = "",
        tyIsUnion = False,
        tyCons = deriveConsRep (Proxy @c) (toFieldRep options src)
      }
  deriveTypeDefinition options _ = [deriveConsRep (Proxy @c) (conRep options (Proxy @f))]

deriveConsRep ::
  Constructor (c :: Meta) =>
  f c ->
  [FieldRep v] ->
  ConsRep v
deriveConsRep proxy fields = ConsRep {..}
  where
    consName = conNameProxy proxy
    consFields
      | isRecordProxy proxy = fields
      | otherwise = enumerate fields

class DeriveFieldRep (gql :: Type -> Constraint) (c :: Type -> Constraint) (v :: Type) f where
  toFieldRep :: DeriveValueOptions kind gql c v -> f a -> [FieldRep v]
  conRep :: DeriveTypeOptions kind gql c v -> proxy f -> [FieldRep v]

instance (DeriveFieldRep gql c v a, DeriveFieldRep gql c v b) => DeriveFieldRep gql c v (a :*: b) where
  toFieldRep options (a :*: b) = toFieldRep options a <> toFieldRep options b
  conRep options _ = conRep options (Proxy @a) <> conRep options (Proxy @b)

instance (Selector s, gql a, c a) => DeriveFieldRep gql c v (M1 S s (Rec0 a)) where
  toFieldRep DeriveValueOptions {..} (M1 (K1 src)) =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef = TypeRef gqlTypeName gqlWrappers,
          fieldValue = __valueApply src
        }
    ]
    where
      TypeData {gqlTypeName, gqlWrappers} = __valueGetType (Proxy @a)
  conRep DeriveTypeOptions {..} _ =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef = TypeRef gqlTypeName gqlWrappers,
          fieldValue = __typeApply (Proxy @a)
        }
    ]
    where
      TypeData {gqlTypeName, gqlWrappers} = __typeGetType (Proxy @a)

instance DeriveFieldRep gql c v U1 where
  toFieldRep _ _ = []
  conRep _ _ = []
