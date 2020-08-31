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
    repToValues,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Internal.Utils
  ( Namespace (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
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
    concatMap,
    map,
    null,
    otherwise,
    show,
    undefined,
    zipWith,
  )

datatypeNameProxy :: forall f (d :: Meta). Datatype d => f d -> TypeName
datatypeNameProxy _ = TypeName $ pack $ datatypeName (undefined :: (M1 D d f a))

conNameProxy :: forall f (c :: Meta). Constructor c => f c -> TypeName
conNameProxy _ = TypeName $ pack $ conName (undefined :: M1 C c U1 a)

selNameProxy :: forall f (s :: Meta). Selector s => f s -> FieldName
selNameProxy _ = convertToJSONName $ FieldName $ pack $ selName (undefined :: M1 S s f a)

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
genericTo f proxy =
  map (stripNamespace (getNamespace proxy)) $
    typeRep f (Proxy @(Rep a))

toValue ::
  forall constraint value (a :: *).
  (GQLType a, Generic a, TypeRep constraint value (Rep a)) =>
  TypeConstraint constraint value Identity ->
  a ->
  DataType value
toValue f =
  stripNamespace (getNamespace (Proxy @a))
    . toTypeRep f
    . from

--  GENERIC UNION
class TypeRep (c :: * -> Constraint) (v :: *) f where
  typeRep :: TypeConstraint c v Proxy -> proxy f -> [ConsRep v]
  toTypeRep :: TypeConstraint c v Identity -> f a -> DataType v

instance (Datatype d, TypeRep c v f) => TypeRep c v (M1 D d f) where
  typeRep fun _ = typeRep fun (Proxy @f)
  toTypeRep fun (M1 src) = (toTypeRep fun src) {tyName = datatypeNameProxy (Proxy @d)}

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep c v a, TypeRep c v b) => TypeRep c v (a :+: b) where
  typeRep fun _ = typeRep fun (Proxy @a) <> typeRep fun (Proxy @b)
  toTypeRep f (L1 x) = (toTypeRep f x) {tyIsUnion = True}
  toTypeRep f (R1 x) = (toTypeRep f x) {tyIsUnion = True}

instance (ConRep con v f, Constructor c) => TypeRep con v (M1 C c f) where
  typeRep fun _ = [deriveConsRep (Proxy @c) (conRep fun (Proxy @f))]
  toTypeRep f (M1 src) =
    DataType
      { tyName = "",
        tyIsUnion = False,
        tyCons = deriveConsRep (Proxy @c) (toFieldRep f src)
      }

deriveConsRep :: Constructor (c :: Meta) => f c -> [FieldRep v] -> ConsRep v
deriveConsRep proxy fields =
  ConsRep
    { consName = conNameProxy proxy,
      consFields
    }
  where
    consFields
      | isRecordProxy proxy = fields
      | otherwise = enumerate fields

class ConRep (c :: * -> Constraint) (v :: *) f where
  conRep :: TypeConstraint c v Proxy -> proxy f -> [FieldRep v]
  toFieldRep :: TypeConstraint c v Identity -> f a -> [FieldRep v]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep c v a, ConRep c v b) => ConRep c v (a :*: b) where
  conRep fun _ = conRep fun (Proxy @a) <> conRep fun (Proxy @b)
  toFieldRep fun (a :*: b) = toFieldRep fun a <> toFieldRep fun b

instance (Selector s, GQLType a, c a) => ConRep c v (M1 S s (Rec0 a)) where
  conRep (TypeConstraint f) _ = [deriveFieldRep (Proxy @s) (Proxy @a) (f $ Proxy @a)]
  toFieldRep (TypeConstraint f) (M1 (K1 src)) = [deriveFieldRep (Proxy @s) (Proxy @a) (f (Identity src))]

deriveFieldRep ::
  forall f (s :: Meta) g a v.
  (Selector s, GQLType a) =>
  f s ->
  g a ->
  v ->
  FieldRep v
deriveFieldRep pSel proxy v =
  FieldRep
    { fieldSelector = selNameProxy pSel,
      fieldTypeRef =
        TypeRef
          { typeConName = __typeName proxy,
            typeWrappers = __wrappers proxy,
            typeArgs = Nothing
          },
      fieldIsObject = isObjectKind proxy,
      fieldValue = v
    }

instance ConRep c v U1 where
  conRep _ _ = []
  toFieldRep _ _ = []

data DataType (v :: *) = DataType
  { tyName :: TypeName,
    tyIsUnion :: Bool,
    tyCons :: ConsRep v
  }

instance Namespace (DataType v) where
  stripNamespace ns r = r {tyCons = stripNamespace ns (tyCons r)}

data ConsRep (v :: *) = ConsRep
  { consName :: TypeName,
    consFields :: [FieldRep v]
  }

instance Namespace (ConsRep v) where
  stripNamespace p ConsRep {consFields, ..}
    | null consFields = ConsRep {consName = stripNamespace p consName, ..}
    | otherwise = ConsRep {consFields = map (stripNamespace p) consFields, ..}

data FieldRep (a :: *) = FieldRep
  { fieldSelector :: FieldName,
    fieldTypeRef :: TypeRef,
    fieldIsObject :: Bool,
    fieldValue :: a
  }

instance Namespace (FieldRep c) where
  stripNamespace p FieldRep {fieldSelector = fields, ..} = FieldRep {fieldSelector = stripNamespace p fields, ..}

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

repToValues :: [ConsRep v] -> [v]
repToValues = concatMap consToValues
  where
    consToValues = map fieldValue . consFields
