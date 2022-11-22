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

module Data.Morpheus.Server.Deriving.Utils.GRep
  ( GRep (..),
    RepContext (..),
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
  ( TypeRef (..),
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

data RepContext gql fun f result = RepContext
  { optApply :: forall a. fun a => f a -> result,
    optTypeData :: forall proxy a. gql a => proxy a -> TypeData
  }

deriveValue ::
  (Generic a, GRep gql constraint value (Rep a), gql a) =>
  RepContext gql constraint Identity value ->
  a ->
  DataType value
deriveValue options value = (deriveTypeValue options (from value)) {dataTypeName}
  where
    dataTypeName = gqlTypeName (optTypeData options (Identity value))

deriveTypeWith ::
  forall kind gql c v kinded a.
  (GRep gql c v (Rep a)) =>
  RepContext gql c Proxy v ->
  kinded kind a ->
  [ConsRep v]
deriveTypeWith options _ = deriveTypeDefinition options (Proxy @(Rep a))

--  GENERIC UNION
class GRep (gql :: Type -> Constraint) (c :: Type -> Constraint) (v :: Type) f where
  deriveTypeValue :: RepContext gql c Identity v -> f a -> DataType v
  deriveTypeDefinition :: RepContext gql c Proxy v -> proxy f -> [ConsRep v]

instance (Datatype d, GRep gql c v f) => GRep gql c v (M1 D d f) where
  deriveTypeValue options (M1 src) = deriveTypeValue options src
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (GRep gql c v a, GRep gql c v b) => GRep gql c v (a :+: b) where
  deriveTypeValue f (L1 x) = (deriveTypeValue f x) {tyIsUnion = True}
  deriveTypeValue f (R1 x) = (deriveTypeValue f x) {tyIsUnion = True}
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @a) <> deriveTypeDefinition options (Proxy @b)

instance (DeriveFieldRep gql con v f, Constructor c) => GRep gql con v (M1 C c f) where
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
  toFieldRep :: RepContext gql c Identity v -> f a -> [FieldRep v]
  conRep :: RepContext gql c Proxy v -> proxy f -> [FieldRep v]

instance (DeriveFieldRep gql c v a, DeriveFieldRep gql c v b) => DeriveFieldRep gql c v (a :*: b) where
  toFieldRep options (a :*: b) = toFieldRep options a <> toFieldRep options b
  conRep options _ = conRep options (Proxy @a) <> conRep options (Proxy @b)

instance (Selector s, gql a, c a) => DeriveFieldRep gql c v (M1 S s (Rec0 a)) where
  toFieldRep RepContext {..} (M1 (K1 src)) =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef = TypeRef gqlTypeName gqlWrappers,
          fieldValue = optApply (Identity src)
        }
    ]
    where
      TypeData {gqlTypeName, gqlWrappers} = optTypeData (Proxy @a)
  conRep RepContext {..} _ =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef = TypeRef gqlTypeName gqlWrappers,
          fieldValue = optApply (Proxy @a)
        }
    ]
    where
      TypeData {gqlTypeName, gqlWrappers} = optTypeData (Proxy @a)

instance DeriveFieldRep gql c v U1 where
  toFieldRep _ _ = []
  conRep _ _ = []
