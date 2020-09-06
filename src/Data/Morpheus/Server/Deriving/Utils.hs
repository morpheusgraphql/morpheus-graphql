{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils
  ( datatypeNameProxy,
    conNameProxy,
    isRecordProxy,
    selNameProxy,
    ResRep (..),
    TypeRep (..),
    ConsRep (..),
    TypeConstraint (..),
    FieldRep (..),
    isEmptyConstraint,
    genericTo,
    DataType (..),
    deriveFieldRep,
    ConRep (..),
    toValue,
    isUnionRef,
    fieldTypeName,
  )
where

import Data.Functor (Functor (..))
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLTypeOptions (..),
    TypeData (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    TypeName (..),
    TypeRef (..),
    convertToJSONName,
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.Text
  ( pack,
  )
import GHC.Exts (Constraint)
import GHC.Generics
  ( (:*:) (..),
    (:+:) (..),
    C,
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
    conIsRecord,
    conName,
    datatypeName,
    selName,
  )
import Prelude
  ( ($),
    (.),
    Bool (..),
    Eq (..),
    Int,
    Maybe (..),
    otherwise,
    show,
    undefined,
    zipWith,
  )

datatypeNameProxy :: forall f (d :: Meta). Datatype d => f d -> TypeName
datatypeNameProxy _ = TypeName $ pack $ datatypeName (undefined :: (M1 D d f a))

conNameProxy :: forall f (c :: Meta). Constructor c => GQLTypeOptions -> f c -> TypeName
conNameProxy GQLTypeOptions {constructorTagModifier} _ =
  TypeName $ pack $ constructorTagModifier $ conName (undefined :: M1 C c U1 a)

selNameProxy :: forall f (s :: Meta). Selector s => GQLTypeOptions -> f s -> FieldName
selNameProxy GQLTypeOptions {fieldLabelModifier} _ =
  convertToJSONName $ FieldName $ pack $ fieldLabelModifier $ selName (undefined :: M1 S s f a)

isRecordProxy :: forall f (c :: Meta). Constructor c => f c -> Bool
isRecordProxy _ = conIsRecord (undefined :: (M1 C c f a))

newtype TypeConstraint (c :: * -> Constraint) (v :: *) (f :: * -> *) = TypeConstraint
  { typeConstraint :: forall a. c a => f a -> v
  }

genericTo ::
  forall f constraint value (a :: *).
  (GQLType a, TypeRep constraint value (Rep a)) =>
  TypeConstraint constraint value Proxy ->
  f a ->
  [ConsRep value]
genericTo f proxy = typeRep (typeOptions proxy, f) (Proxy @(Rep a))

toValue ::
  forall constraint value (a :: *).
  (GQLType a, Generic a, TypeRep constraint value (Rep a)) =>
  TypeConstraint constraint value Identity ->
  a ->
  DataType value
toValue f = toTypeRep (typeOptions (Proxy @a), f) . from

--  GENERIC UNION
class TypeRep (c :: * -> Constraint) (v :: *) f where
  typeRep :: (GQLTypeOptions, TypeConstraint c v Proxy) -> proxy f -> [ConsRep v]
  toTypeRep :: (GQLTypeOptions, TypeConstraint c v Identity) -> f a -> DataType v

instance (Datatype d, TypeRep c v f) => TypeRep c v (M1 D d f) where
  typeRep fun _ = typeRep fun (Proxy @f)
  toTypeRep fun (M1 src) = (toTypeRep fun src) {tyName = datatypeNameProxy (Proxy @d)}

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep c v a, TypeRep c v b) => TypeRep c v (a :+: b) where
  typeRep fun _ = typeRep fun (Proxy @a) <> typeRep fun (Proxy @b)
  toTypeRep f (L1 x) = (toTypeRep f x) {tyIsUnion = True}
  toTypeRep f (R1 x) = (toTypeRep f x) {tyIsUnion = True}

instance (ConRep con v f, Constructor c) => TypeRep con v (M1 C c f) where
  typeRep f@(opt, _) _ = [deriveConsRep opt (Proxy @c) (conRep f (Proxy @f))]
  toTypeRep f@(opt, _) (M1 src) =
    DataType
      { tyName = "",
        tyIsUnion = False,
        tyCons = deriveConsRep opt (Proxy @c) (toFieldRep f src)
      }

deriveConsRep ::
  Constructor (c :: Meta) =>
  GQLTypeOptions ->
  f c ->
  [FieldRep v] ->
  ConsRep v
deriveConsRep opt proxy fields =
  ConsRep
    { consName = conNameProxy opt proxy,
      consFields
    }
  where
    consFields
      | isRecordProxy proxy = fields
      | otherwise = enumerate fields

class ConRep (c :: * -> Constraint) (v :: *) f where
  conRep :: (GQLTypeOptions, TypeConstraint c v Proxy) -> proxy f -> [FieldRep v]
  toFieldRep :: (GQLTypeOptions, TypeConstraint c v Identity) -> f a -> [FieldRep v]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep c v a, ConRep c v b) => ConRep c v (a :*: b) where
  conRep fun _ = conRep fun (Proxy @a) <> conRep fun (Proxy @b)
  toFieldRep fun (a :*: b) = toFieldRep fun a <> toFieldRep fun b

instance (Selector s, GQLType a, c a) => ConRep c v (M1 S s (Rec0 a)) where
  conRep (opt, TypeConstraint f) _ = [deriveFieldRep opt (Proxy @s) (Proxy @a) (f $ Proxy @a)]
  toFieldRep (opt, TypeConstraint f) (M1 (K1 src)) = [deriveFieldRep opt (Proxy @s) (Proxy @a) (f (Identity src))]

deriveFieldRep ::
  forall f (s :: Meta) g a v.
  (Selector s, GQLType a) =>
  GQLTypeOptions ->
  f s ->
  g a ->
  v ->
  FieldRep v
deriveFieldRep opt pSel proxy v =
  FieldRep
    { fieldSelector = selNameProxy opt pSel,
      fieldTypeRef =
        TypeRef
          { typeConName = gqlTypeName,
            typeWrappers = gqlWrappers,
            typeArgs = Nothing
          },
      fieldIsObject = isObjectKind proxy,
      fieldValue = v
    }
  where
    TypeData {gqlTypeName, gqlWrappers} = __type proxy

instance ConRep c v U1 where
  conRep _ _ = []
  toFieldRep _ _ = []

data DataType (v :: *) = DataType
  { tyName :: TypeName,
    tyIsUnion :: Bool,
    tyCons :: ConsRep v
  }

data ConsRep (v :: *) = ConsRep
  { consName :: TypeName,
    consFields :: [FieldRep v]
  }

data FieldRep (a :: *) = FieldRep
  { fieldSelector :: FieldName,
    fieldTypeRef :: TypeRef,
    fieldIsObject :: Bool,
    fieldValue :: a
  }
  deriving (Functor)

data ResRep (a :: *) = ResRep
  { enumCons :: [TypeName],
    unionRef :: [TypeName],
    unionRecordRep :: [ConsRep a]
  }

isEmptyConstraint :: ConsRep a -> Bool
isEmptyConstraint ConsRep {consFields = []} = True
isEmptyConstraint _ = False

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
enumerate :: [FieldRep a] -> [FieldRep a]
enumerate = zipWith setFieldName ([0 ..] :: [Int])
  where
    setFieldName i field = field {fieldSelector = FieldName $ "_" <> pack (show i)}

fieldTypeName :: FieldRep k -> TypeName
fieldTypeName = typeConName . fieldTypeRef

isUnionRef :: TypeName -> ConsRep k -> Bool
isUnionRef baseName ConsRep {consName, consFields = [fieldRep@FieldRep {fieldIsObject = True}]} =
  consName == baseName <> fieldTypeName fieldRep
isUnionRef _ _ = False
