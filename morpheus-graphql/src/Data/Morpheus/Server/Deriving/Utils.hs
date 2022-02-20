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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils
  ( conNameProxy,
    isRecordProxy,
    selNameProxy,
    TypeRep (..),
    ConsRep (..),
    TypeConstraint (..),
    FieldRep (..),
    isEmptyConstraint,
    DataType (..),
    ConRep (..),
    toRep,
    toValue,
    isUnionRef,
    fieldTypeName,
    unpackMonad,
    deriveTypeRef,
    symbolName,
    withKind,
    toFieldRes,
  )
where

import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CategoryValue (..),
    KindedProxy (KindedProxy),
    kinded,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLTypeOptions (..),
    TypeData (..),
    __typeData,
    defaultTypeOptions,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeCategory,
    TypeName,
    TypeRef (..),
    packName,
  )
import Data.Text
  ( pack,
  )
import qualified Data.Text as T
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
    selName,
  )
import GHC.TypeLits
import Relude hiding (undefined)
import Prelude (undefined)

conNameProxy :: forall f (c :: Meta). Constructor c => GQLTypeOptions -> f c -> TypeName
conNameProxy options _ =
  packName $ pack $ constructorTagModifier options $ conName (undefined :: M1 C c U1 a)

selNameProxy :: forall f (s :: Meta). Selector s => GQLTypeOptions -> f s -> FieldName
selNameProxy options _ =
  fromHaskellName
    $ fieldLabelModifier options
    $ selName (undefined :: M1 S s f a)

fromHaskellName :: String -> FieldName
fromHaskellName hsName
  | not (null hsName) && (T.last name == '\'') = packName (T.init name)
  | otherwise = packName name
  where
    name = T.pack hsName
{-# INLINE fromHaskellName #-}

isRecordProxy :: forall f (c :: Meta). Constructor c => f c -> Bool
isRecordProxy _ = conIsRecord (undefined :: (M1 C c f a))

newtype TypeConstraint (c :: Type -> Constraint) (v :: Type) (f :: Type -> Type) = TypeConstraint
  { typeConstraint :: forall a. c a => f a -> v
  }

toRep ::
  forall kinded constraint value (a :: Type) (kind :: TypeCategory).
  (GQLType a, CategoryValue kind, TypeRep constraint value (Rep a)) =>
  TypeConstraint constraint value Proxy ->
  kinded kind a ->
  [ConsRep value]
toRep f proxy = typeRep (typeOptions proxy defaultTypeOptions, Proxy @kind, f) (Proxy @(Rep a))

toValue ::
  forall proxy (kind :: TypeCategory) constraint value (a :: *).
  (GQLType a, CategoryValue kind, Generic a, TypeRep constraint value (Rep a)) =>
  TypeConstraint constraint value Identity ->
  proxy kind ->
  a ->
  DataType value
toValue f proxy = toTypeRep (typeName, options, proxy, f) . from
  where
    typeName = gqlTypeName $ __typeData (KindedProxy :: KindedProxy kind a)
    options = typeOptions (Proxy @a) defaultTypeOptions

--  GENERIC UNION
class TypeRep (c :: Type -> Constraint) (v :: Type) f where
  typeRep :: CategoryValue kind => (GQLTypeOptions, kinProxy (kind :: TypeCategory), TypeConstraint c v Proxy) -> proxy f -> [ConsRep v]
  toTypeRep :: CategoryValue kind => (TypeName, GQLTypeOptions, kinProxy (kind :: TypeCategory), TypeConstraint c v Identity) -> f a -> DataType v

instance (Datatype d, TypeRep c v f) => TypeRep c v (M1 D d f) where
  typeRep fun _ = typeRep fun (Proxy @f)
  toTypeRep fun@(dataTypeName, _, _, _) (M1 src) = (toTypeRep fun src) {dataTypeName}

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep c v a, TypeRep c v b) => TypeRep c v (a :+: b) where
  typeRep fun _ = typeRep fun (Proxy @a) <> typeRep fun (Proxy @b)
  toTypeRep f (L1 x) = (toTypeRep f x) {tyIsUnion = True}
  toTypeRep f (R1 x) = (toTypeRep f x) {tyIsUnion = True}

instance (ConRep con v f, Constructor c) => TypeRep con v (M1 C c f) where
  typeRep f@(opt, _, _) _ = [deriveConsRep opt (Proxy @c) (conRep f (Proxy @f))]
  toTypeRep (_, opt, x, y) (M1 src) =
    DataType
      { dataTypeName = "",
        tyIsUnion = False,
        tyCons = deriveConsRep opt (Proxy @c) (toFieldRep (opt, x, y) src)
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
  conRep :: CategoryValue kind => (GQLTypeOptions, kinProxy (kind :: TypeCategory), TypeConstraint c v Proxy) -> proxy f -> [FieldRep v]
  toFieldRep :: CategoryValue kind => (GQLTypeOptions, kinProxy (kind :: TypeCategory), TypeConstraint c v Identity) -> f a -> [FieldRep v]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep c v a, ConRep c v b) => ConRep c v (a :*: b) where
  conRep fun _ = conRep fun (Proxy @a) <> conRep fun (Proxy @b)
  toFieldRep fun (a :*: b) = toFieldRep fun a <> toFieldRep fun b

instance (Selector s, GQLType a, c a) => ConRep c v (M1 S s (Rec0 a)) where
  conRep (opt, kind, TypeConstraint f) _ = [deriveFieldRep opt (Proxy @s) (kinded kind (Proxy @a)) (f $ Proxy @a)]
  toFieldRep (opt, kind, TypeConstraint f) (M1 (K1 src)) = [deriveFieldRep opt (Proxy @s) (kinded kind (Proxy @a)) (f (Identity src))]

deriveFieldRep ::
  forall
    proxy
    (selector :: Meta)
    (kindedProxy :: TypeCategory -> * -> *)
    a
    v
    (kind :: TypeCategory).
  ( Selector selector,
    GQLType a,
    CategoryValue kind
  ) =>
  GQLTypeOptions ->
  proxy selector ->
  kindedProxy kind a ->
  v ->
  FieldRep v
deriveFieldRep opt pSel kindedProxy v =
  FieldRep
    { fieldSelector = selNameProxy opt pSel,
      fieldTypeRef = deriveTypeRef kindedProxy,
      fieldValue = v
    }

deriveTypeRef :: (GQLType a, CategoryValue kind) => kinded kind a -> TypeRef
deriveTypeRef kindedProxy =
  TypeRef
    { typeConName = gqlTypeName,
      typeWrappers = gqlWrappers
    }
  where
    TypeData {gqlTypeName, gqlWrappers} = __typeData kindedProxy

instance ConRep c v U1 where
  conRep _ _ = []
  toFieldRep _ _ = []

data DataType (v :: Type) = DataType
  { dataTypeName :: TypeName,
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
    fieldValue :: a
  }
  deriving (Functor)

toFieldRes :: FieldRep (m a) -> (FieldName, m a)
toFieldRes FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)

unpackMonadFromField :: Monad m => FieldRep (m a) -> m (FieldRep a)
unpackMonadFromField FieldRep {..} = do
  cont <- fieldValue
  pure (FieldRep {fieldValue = cont, ..})

unpackMonadFromCons :: Monad m => ConsRep (m a) -> m (ConsRep a)
unpackMonadFromCons ConsRep {..} = ConsRep consName <$> traverse unpackMonadFromField consFields

unpackMonad :: Monad m => [ConsRep (m a)] -> m [ConsRep a]
unpackMonad = traverse unpackMonadFromCons

isEmptyConstraint :: ConsRep a -> Bool
isEmptyConstraint ConsRep {consFields = []} = True
isEmptyConstraint _ = False

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
enumerate :: [FieldRep a] -> [FieldRep a]
enumerate = zipWith setFieldName ([0 ..] :: [Int])
  where
    setFieldName i field = field {fieldSelector = packName $ "_" <> pack (show i)}

fieldTypeName :: FieldRep k -> TypeName
fieldTypeName = typeConName . fieldTypeRef

isUnionRef :: TypeName -> ConsRep k -> Bool
isUnionRef baseName ConsRep {consName, consFields = [fieldRep]} =
  consName == baseName <> fieldTypeName fieldRep
isUnionRef _ _ = False

symbolName :: KnownSymbol a => f a -> FieldName
symbolName = fromString . symbolVal

withKind :: Proxy a -> KindedProxy (KIND a) a
withKind _ = KindedProxy
