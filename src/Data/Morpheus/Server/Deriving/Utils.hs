{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  )
where

import Data.Morpheus.Internal.Utils
  ( Namespace (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    TypeUpdater,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    FieldDefinition (..),
    FieldName,
    FieldName (..),
    TypeCategory,
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
import Prelude
  ( ($),
    (.),
    Bool,
    map,
    undefined,
  )

datatypeNameProxy :: forall f (d :: Meta). Datatype d => f d -> TypeName
datatypeNameProxy _ = TypeName $ pack $ datatypeName (undefined :: (M1 D d f a))

conNameProxy :: forall f (c :: Meta). Constructor c => f c -> TypeName
conNameProxy _ = TypeName $ pack $ conName (undefined :: M1 C c U1 a)

selNameProxy :: forall f (s :: Meta). Selector s => f s -> FieldName
selNameProxy _ = convertToJSONName $ FieldName $ pack $ selName (undefined :: M1 S s f a)

isRecordProxy :: forall f (c :: Meta). Constructor c => f c -> Bool
isRecordProxy _ = conIsRecord (undefined :: (M1 C c f a))

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

newtype TypeConstraint (kind :: TypeCategory) (c :: * -> Constraint) f
  = TypeConstraint
      ( forall a proxy.
        c a =>
        ( FieldName -> proxy kind a -> FieldDefinition kind CONST,
          proxy kind a -> TypeUpdater
        )
      )

mapConstraint :: f b -> TypeConstraint k c a -> TypeConstraint k c b
mapConstraint _ (TypeConstraint f) = TypeConstraint f

--  GENERIC UNION
class TypeRep (kind :: TypeCategory) (c :: * -> Constraint) f where
  typeRep :: TypeConstraint kind c f -> [ConsRep kind]

instance TypeRep kind c f => TypeRep kind c (M1 D d f) where
  typeRep = typeRep . mapConstraint (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep kind c a, TypeRep kind c b) => TypeRep kind c (a :+: b) where
  typeRep (TypeConstraint f) = typeRep (TypeConstraint f :: TypeConstraint kind c a) <> typeRep (TypeConstraint f :: TypeConstraint kind c b)

instance (ConRep kind con f, Constructor c) => TypeRep kind con (M1 C c f) where
  typeRep (TypeConstraint f) =
    [ ConsRep
        { consName = conNameProxy (Proxy @c),
          consFields = conRep (TypeConstraint f :: TypeConstraint kind con f),
          consIsRecord = isRecordProxy (Proxy @c)
        }
    ]

class ConRep (kind :: TypeCategory) (c :: * -> Constraint) f where
  conRep :: TypeConstraint kind c f -> [FieldRep kind]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep kind c a, ConRep kind c b) => ConRep kind c (a :*: b) where
  conRep (TypeConstraint f) =
    conRep (TypeConstraint f :: TypeConstraint kind c a)
      <> conRep (TypeConstraint f :: TypeConstraint kind c b)

instance (Selector s, GQLType a, c a) => ConRep kind c (M1 S s (Rec0 a)) where
  conRep (TypeConstraint (f, t)) =
    [ FieldRep
        { fieldTypeName = typeConName $ fieldType fieldData,
          fieldData = fieldData,
          fieldTypeUpdater = t (KindedProxy :: KindedProxy cat a),
          fieldIsObject = isObjectKind (Proxy @a)
        }
    ]
    where
      name = selNameProxy (Proxy @s)
      fieldData = f name (KindedProxy :: KindedProxy kind a)

instance ConRep kind c U1 where
  conRep _ = []

data ConsRep kind = ConsRep
  { consName :: TypeName,
    consIsRecord :: Bool,
    consFields :: [FieldRep kind]
  }

instance Namespace (ConsRep kind) where
  stripNamespace p ConsRep {consFields = fields, ..} = ConsRep {consFields = map (stripNamespace p) fields, ..}

data FieldRep kind = FieldRep
  { fieldTypeName :: TypeName,
    fieldData :: FieldDefinition kind CONST,
    fieldTypeUpdater :: TypeUpdater,
    fieldIsObject :: Bool
  }

instance Namespace (FieldRep kind) where
  stripNamespace p FieldRep {fieldData = fields, ..} = FieldRep {fieldData = stripNamespace p fields, ..}

data ResRep kind = ResRep
  { enumCons :: [TypeName],
    unionRef :: [TypeName],
    unionRecordRep :: [ConsRep kind]
  }
