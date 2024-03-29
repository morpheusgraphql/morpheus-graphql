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

module Data.Morpheus.Generic.GRep
  ( GRep (..),
    GRepFun (..),
    GRepCons (..),
    GRepField (..),
    GRepValue (..),
    GRepType (..),
    deriveValue,
    deriveType,
  )
where

import Data.List (partition)
import Data.Morpheus.Generic.Proxy
  ( conNameP,
    isRecordP,
    selNameP,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
    TypeRef (..),
    TypeWrapper,
    packName,
  )
import Data.Text (pack)
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

data GRepFun gql fun f result = GRepFun
  { grepFun :: forall a. (fun a) => f a -> result,
    grepTypename :: forall proxy a. (gql a) => proxy a -> TypeName,
    grepWrappers :: forall proxy a. (gql a) => proxy a -> TypeWrapper
  }

deriveValue ::
  (Generic a, GRep gql constraint value (Rep a), gql a) =>
  GRepFun gql constraint Identity value ->
  a ->
  GRepValue value
deriveValue options value
  | null cons = GRepValueEnum typename (consName cons)
  | isUnion = case (isUnionRef typename cons, consFields cons) of
      (True, [GRepField {..}]) -> GRepValueUnionRef (typeConName fieldTypeRef) fieldValue
      _ -> GRepValueUnion typename (consName cons) (consFields cons)
  | otherwise = GRepValueObject typename (consFields cons)
  where
    (isUnion, cons) = deriveTypeValue options (from value)
    typename = grepTypename options (Identity value)

toRep :: f a -> Proxy (Rep a)
toRep _ = Proxy

deriveType ::
  forall kind gql c v kinded m a.
  (GRep gql c (m v) (Rep a), Monad m, gql a) =>
  GRepFun gql c Proxy (m v) ->
  kinded kind a ->
  m (GRepType v)
deriveType ctx x = toType <$> unpackMonad (deriveTypeDefinition ctx (toRep x))
  where
    toType cons | all null cons = GRepTypeEnum (consName <$> cons)
    toType [GRepCons {consFields}] = GRepTypeObject consFields
    toType cons = GRepTypeUnion unionRef unionCons
      where
        unionRef = toVer <$> concatMap consFields unionRefRep
          where
            toVer GRepField {..} = (typeConName fieldTypeRef, fieldValue)
        --
        (unionRefRep, unionCons) = partition (isUnionRef typename) cons
        typename = grepTypename ctx x

--  GENERIC UNION
class GRep (gql :: Type -> Constraint) (c :: Type -> Constraint) (v :: Type) f where
  deriveTypeValue :: GRepFun gql c Identity v -> f a -> (Bool, GRepCons v)
  deriveTypeDefinition :: GRepFun gql c Proxy v -> proxy f -> [GRepCons v]

instance (Datatype d, GRep gql c v f) => GRep gql c v (M1 D d f) where
  deriveTypeValue options (M1 src) = deriveTypeValue options src
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (GRep gql c v a, GRep gql c v b) => GRep gql c v (a :+: b) where
  deriveTypeValue f (L1 x) = (True, snd (deriveTypeValue f x))
  deriveTypeValue f (R1 x) = (True, snd (deriveTypeValue f x))
  deriveTypeDefinition options _ = deriveTypeDefinition options (Proxy @a) <> deriveTypeDefinition options (Proxy @b)

instance (DeriveFieldRep gql con v f, Constructor c) => GRep gql con v (M1 C c f) where
  deriveTypeValue options (M1 src) = (False, deriveConsRep (Proxy @c) (toFieldRep options src))
  deriveTypeDefinition options _ = [deriveConsRep (Proxy @c) (conRep options (Proxy @f))]

deriveConsRep ::
  (Constructor (c :: Meta)) =>
  f c ->
  [GRepField v] ->
  GRepCons v
deriveConsRep proxy fields = GRepCons {..}
  where
    consName = conNameP proxy
    consFields
      | isRecordP proxy = fields
      | otherwise = enumerate fields

class DeriveFieldRep (gql :: Type -> Constraint) (c :: Type -> Constraint) (v :: Type) f where
  toFieldRep :: GRepFun gql c Identity v -> f a -> [GRepField v]
  conRep :: GRepFun gql c Proxy v -> proxy f -> [GRepField v]
  scanRec :: GRepFun gql c Proxy v -> proxy f -> [v]

instance (DeriveFieldRep gql c v a, DeriveFieldRep gql c v b) => DeriveFieldRep gql c v (a :*: b) where
  toFieldRep options (a :*: b) = toFieldRep options a <> toFieldRep options b
  conRep options _ = conRep options (Proxy @a) <> conRep options (Proxy @b)
  scanRec ctx _ = scanRec ctx (Proxy @a) <> scanRec ctx (Proxy @b)

instance (Selector s, gql a, c a) => DeriveFieldRep gql c v (M1 S s (Rec0 a)) where
  toFieldRep GRepFun {..} (M1 (K1 src)) =
    [ GRepField
        { fieldSelector = selNameP (Proxy @s),
          fieldTypeRef = TypeRef (grepTypename (Proxy @a)) (grepWrappers (Proxy @a)),
          fieldValue = grepFun (Identity src)
        }
    ]

  conRep GRepFun {..} _ =
    [ GRepField
        { fieldSelector = selNameP (Proxy @s),
          fieldTypeRef = TypeRef (grepTypename (Proxy @a)) (grepWrappers (Proxy @a)),
          fieldValue = grepFun (Proxy @a)
        }
    ]
  scanRec GRepFun {..} _ = [grepFun (Proxy @a)]

instance DeriveFieldRep gql c v U1 where
  toFieldRep _ _ = []
  conRep _ _ = []
  scanRec _ _ = []

data GRepType v
  = GRepTypeEnum [TypeName]
  | GRepTypeObject [GRepField v]
  | GRepTypeUnion
      { variantRefs :: [(TypeName, v)],
        inlineVariants :: [GRepCons v]
      }

instance Foldable GRepType where
  foldr _ res GRepTypeEnum {} = res
  foldr f res (GRepTypeObject fields) = foldr f res (map fieldValue fields)
  foldr f res GRepTypeUnion {inlineVariants, variantRefs} = foldr f res (concatMap toList inlineVariants <> map snd variantRefs)

data GRepValue v
  = GRepValueUnionRef
      { unionRefTypeName :: TypeName,
        unionRefValue :: v
      }
  | GRepValueUnion
      { unionTypeName :: TypeName,
        unionVariantName :: TypeName,
        unionFields :: [GRepField v]
      }
  | GRepValueObject
      { objectTypeName :: TypeName,
        objectFields :: [GRepField v]
      }
  | GRepValueEnum
      { enumTypeName :: TypeName,
        enumVariantName :: TypeName
      }
  deriving (Functor)

data GRepCons (a :: Type) = GRepCons
  { consName :: TypeName,
    consFields :: [GRepField a]
  }
  deriving (Functor)

instance Foldable GRepCons where
  foldMap f GRepCons {..} = foldMap f (map fieldValue consFields)

data GRepField (a :: Type) = GRepField
  { fieldSelector :: FieldName,
    fieldTypeRef :: TypeRef,
    fieldValue :: a
  }
  deriving (Functor)

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
enumerate :: [GRepField a] -> [GRepField a]
enumerate = zipWith setFieldName ([0 ..] :: [Int])
  where
    setFieldName i field = field {fieldSelector = packName $ "_" <> pack (show i)}

isUnionRef :: TypeName -> GRepCons k -> Bool
isUnionRef baseName GRepCons {consName, consFields = [fieldRep]} =
  consName == baseName <> typeConName (fieldTypeRef fieldRep)
isUnionRef _ _ = False

unpackMonad :: (Monad m) => [GRepCons (m a)] -> m [GRepCons a]
unpackMonad = traverse unpackMonadFromCons

unpackMonadFromField :: (Monad m) => GRepField (m a) -> m (GRepField a)
unpackMonadFromField GRepField {..} = do
  cont <- fieldValue
  pure (GRepField {fieldValue = cont, ..})

unpackMonadFromCons :: (Monad m) => GRepCons (m a) -> m (GRepCons a)
unpackMonadFromCons GRepCons {..} = GRepCons consName <$> traverse unpackMonadFromField consFields
