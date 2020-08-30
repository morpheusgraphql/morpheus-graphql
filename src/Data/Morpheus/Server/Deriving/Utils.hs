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

newtype
  TypeConstraint
    (kind :: TypeCategory)
    (c :: * -> Constraint)
    (v :: *)
    f = TypeConstraint
  { typeConstraintFun :: forall a proxy. c a => proxy kind a -> v
  }

mapConstraint :: f b -> TypeConstraint k c v a -> TypeConstraint k c v b
mapConstraint _ (TypeConstraint f) = TypeConstraint f

--  GENERIC UNION
class TypeRep (kind :: TypeCategory) (c :: * -> Constraint) (v :: *) f where
  typeRep :: TypeConstraint kind c v f -> [ConsRep kind v]

instance TypeRep kind c v f => TypeRep kind c v (M1 D d f) where
  typeRep = typeRep . mapConstraint (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep kind c v a, TypeRep kind c v b) => TypeRep kind c v (a :+: b) where
  typeRep (TypeConstraint f) = typeRep (TypeConstraint f :: TypeConstraint kind c v a) <> typeRep (TypeConstraint f :: TypeConstraint kind c v b)

instance (ConRep kind con v f, Constructor c) => TypeRep kind con v (M1 C c f) where
  typeRep (TypeConstraint f) =
    [ ConsRep
        { consName = conNameProxy (Proxy @c),
          consFields = conRep (TypeConstraint f :: TypeConstraint kind con v f),
          consIsRecord = isRecordProxy (Proxy @c)
        }
    ]

class ConRep (kind :: TypeCategory) (c :: * -> Constraint) (v :: *) f where
  conRep :: TypeConstraint kind c v f -> [FieldRep kind v]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep kind c v a, ConRep kind c v b) => ConRep kind c v (a :*: b) where
  conRep (TypeConstraint f) =
    conRep (TypeConstraint f :: TypeConstraint kind c v a)
      <> conRep (TypeConstraint f :: TypeConstraint kind c v b)

instance (Selector s, GQLType a, c a) => ConRep kind c v (M1 S s (Rec0 a)) where
  conRep (TypeConstraint f) =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldValue = f (KindedProxy :: KindedProxy cat a),
          fieldIsObject = isObjectKind (Proxy @a)
        }
    ]

instance ConRep kind c v U1 where
  conRep _ = []

data ConsRep kind (v :: *) = ConsRep
  { consName :: TypeName,
    consIsRecord :: Bool,
    consFields :: [FieldRep kind v]
  }

-- instance Namespace (ConsRep kind v) where
--   stripNamespace p ConsRep {consFields = fields, ..} = ConsRep {consFields = map (stripNamespace p) fields, ..}

data FieldRep kind (value :: *) = FieldRep
  { fieldSelector :: FieldName,
    fieldIsObject :: Bool,
    fieldValue :: value
  }

-- instance Namespace (FieldRep kind) where
--   stripNamespace p FieldRep {fieldValue = fields, ..} = FieldRep {fieldValue = stripNamespace p fields, ..}

data ResRep kind (v :: *) = ResRep
  { enumCons :: [TypeName],
    unionRef :: [TypeName],
    unionRecordRep :: [ConsRep kind v]
  }
