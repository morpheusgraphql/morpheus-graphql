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

import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CategoryValue (..),
  )
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

data DeriveValueOptions kind c v = DeriveValueOptions
  { __valueTypeName :: TypeName,
    __valueApply :: forall a. c a => a -> v,
    __valueGetType :: forall f a. c a => f a -> TypeData
  }

data DeriveTypeOptions kind c v = DeriveTypeDefinitionOptions
  { __typeApply :: forall f a. c a => f a -> v,
    __typeGetType :: forall f a. c a => f a -> TypeData
  }

deriveValue ::
  (CategoryValue kind, Generic a, DeriveWith constraint value (Rep a)) =>
  DeriveValueOptions kind constraint value ->
  a ->
  DataType value
deriveValue options = deriveTypeValue options . from

deriveTypeWith ::
  forall kind c v kinded a.
  (CategoryValue kind, DeriveWith c v (Rep a)) =>
  DeriveTypeOptions kind c v ->
  kinded kind a ->
  [ConsRep v]
deriveTypeWith options _ = deriveTypeDefinition options (Proxy @(Rep a))

--  GENERIC UNION
class DeriveWith (c :: Type -> Constraint) (v :: Type) f where
  deriveTypeValue :: CategoryValue kind => DeriveValueOptions kind c v -> f a -> DataType v
  deriveTypeDefinition :: CategoryValue kind => DeriveTypeOptions kind c v -> proxy f -> [ConsRep v]

instance (Datatype d, DeriveWith c v f) => DeriveWith c v (M1 D d f) where
  deriveTypeValue options (M1 src) = (deriveTypeValue options src) {dataTypeName = __valueTypeName options}
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (DeriveWith c v a, DeriveWith c v b) => DeriveWith c v (a :+: b) where
  deriveTypeValue f (L1 x) = (deriveTypeValue f x) {tyIsUnion = True}
  deriveTypeValue f (R1 x) = (deriveTypeValue f x) {tyIsUnion = True}
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @a) <> deriveTypeDefinition options (Proxy @b)

instance (DeriveFieldRep con v f, Constructor c) => DeriveWith con v (M1 C c f) where
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

class DeriveFieldRep (c :: Type -> Constraint) (v :: Type) f where
  toFieldRep :: CategoryValue kind => DeriveValueOptions kind c v -> f a -> [FieldRep v]
  conRep :: CategoryValue kind => DeriveTypeOptions kind c v -> proxy f -> [FieldRep v]

instance (DeriveFieldRep c v a, DeriveFieldRep c v b) => DeriveFieldRep c v (a :*: b) where
  toFieldRep options (a :*: b) = toFieldRep options a <> toFieldRep options b
  conRep options _ = conRep options (Proxy @a) <> conRep options (Proxy @b)

instance (Selector s, c a) => DeriveFieldRep c v (M1 S s (Rec0 a)) where
  toFieldRep DeriveValueOptions {..} (M1 (K1 src)) =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef = TypeRef gqlTypeName gqlWrappers,
          fieldValue = __valueApply src
        }
    ]
    where
      TypeData {gqlTypeName, gqlWrappers} = __valueGetType (Proxy @a)
  conRep DeriveTypeDefinitionOptions {..} _ =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef = TypeRef gqlTypeName gqlWrappers,
          fieldValue = __typeApply (Proxy @a)
        }
    ]
    where
      TypeData {gqlTypeName, gqlWrappers} = __typeGetType (Proxy @a)

instance DeriveFieldRep c v U1 where
  toFieldRep _ _ = []
  conRep _ _ = []
