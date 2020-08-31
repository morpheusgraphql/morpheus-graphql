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
    enumerateFieldNames,
    genericTo,
    enumerate,
    DataType (..),
  )
where

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
import Prelude
  ( ($),
    Bool (..),
    Int,
    Maybe (..),
    map,
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

newtype TypeConstraint (c :: * -> Constraint) (v :: *) = TypeConstraint
  { typeConstraint :: forall a f. c a => f a -> v
  }

genericTo ::
  forall f constraint value (a :: *).
  (GQLType a, TypeRep constraint value (Rep a)) =>
  TypeConstraint constraint value ->
  f a ->
  [ConsRep value]
genericTo f proxy =
  map (stripNamespace (getNamespace proxy)) $
    typeRep f (Proxy @(Rep a))

--  GENERIC UNION
class TypeRep (c :: * -> Constraint) (v :: *) f where
  typeRep :: TypeConstraint c v -> proxy f -> [ConsRep v]

instance TypeRep c v f => TypeRep c v (M1 D d f) where
  typeRep fun _ = typeRep fun (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep c v a, TypeRep c v b) => TypeRep c v (a :+: b) where
  typeRep fun _ = typeRep fun (Proxy @a) <> typeRep fun (Proxy @b)

instance (ConRep con v f, Constructor c) => TypeRep con v (M1 C c f) where
  typeRep fun _ =
    [ ConsRep
        { consName = conNameProxy (Proxy @c),
          consFields = conRep fun (Proxy @f),
          consIsRecord = isRecordProxy (Proxy @c)
        }
    ]

class ConRep (c :: * -> Constraint) (v :: *) f where
  conRep :: TypeConstraint c v -> proxy f -> [FieldRep v]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep c v a, ConRep c v b) => ConRep c v (a :*: b) where
  conRep fun _ = conRep fun (Proxy @a) <> conRep fun (Proxy @b)

instance (Selector s, GQLType a, c a) => ConRep c v (M1 S s (Rec0 a)) where
  conRep (TypeConstraint f) _ =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef =
            TypeRef
              { typeConName = __typeName (Proxy @a),
                typeWrappers = __wrappers (Proxy @a),
                typeArgs = Nothing
              },
          fieldIsObject = isObjectKind (Proxy @a),
          fieldValue = f (Proxy @a)
        }
    ]

instance ConRep c v U1 where
  conRep _ _ = []

data DataType (v :: *) = DataType
  { tyName :: TypeName,
    tyIsUnion :: Bool,
    tyCons :: ConsRep v
  }

instance Namespace (DataType v) where
  stripNamespace ns r = r {tyCons = stripNamespace ns (tyCons r)}

data ConsRep (v :: *) = ConsRep
  { consName :: TypeName,
    consIsRecord :: Bool,
    consFields :: [FieldRep v]
  }

instance Namespace (ConsRep v) where
  stripNamespace p ConsRep {consFields = fields, ..} = ConsRep {consFields = map (stripNamespace p) fields, ..}

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

enumerateFieldNames :: ConsRep a -> ConsRep a
enumerateFieldNames cons@ConsRep {consFields} = cons {consFields = enumerate consFields}

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
enumerate :: [FieldRep a] -> [FieldRep a]
enumerate = zipWith setFieldName ([0 ..] :: [Int])
  where
    setFieldName i field = field {fieldSelector = FieldName $ "_" <> pack (show i)}
