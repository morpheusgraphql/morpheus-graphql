{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Execution.Server.Introspect
  ( TypeUpdater
  , Introspect(..)
  , ObjectFields(..)
  , IntroCon
  , updateLib
  , buildType
  )
where

import           Data.Map                       ( Map )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Schema     ( nameCollisionError )
import           Data.Morpheus.Execution.Server.Generics.EnumRep
                                                ( EnumRep(..) )
import           Data.Morpheus.Kind             ( Context(..)
                                                , ENUM
                                                , GQL_KIND
                                                , INPUT_OBJECT
                                                , INPUT_UNION
                                                , OBJECT
                                                , SCALAR
                                                , UNION
                                                )
import           Data.Morpheus.Types.Types      ( MapKind
                                                , Pair
                                                )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar(..) )
import           Data.Morpheus.Types.GQLType    ( GQLType(..) )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..)
                                                , resolveUpdates
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DataArguments
                                                , Meta(..)
                                                , DataField(..)
                                                , DataTyCon(..)
                                                , DataType(..)
                                                , Key
                                                , createAlias
                                                , defineType
                                                , isTypeDefined
                                                , toListField
                                                , toNullableField
                                                , createEnumValue
                                                , TypeUpdater
                                                )

type IntroCon a = (GQLType a, ObjectFields (CUSTOM a) a)

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class Introspect a where
  field :: proxy a -> Text -> DataField
  introspect :: proxy a -> TypeUpdater
  -----------------------------------------------
  default field :: GQLType a =>
    proxy a -> Text -> DataField
  field _ = buildField (Proxy @a) []

instance {-# OVERLAPPABLE #-} (GQLType a, IntrospectKind (KIND a) a) => Introspect a where
  introspect _ = introspectKind (Context :: Context (KIND a) a)

-- Maybe
instance Introspect a => Introspect (Maybe a) where
  field _ = toNullableField . field (Proxy @a)
  introspect _ = introspect (Proxy @a)

-- List
instance Introspect a => Introspect [a] where
  field _ = toListField . field (Proxy @a)
  introspect _ = introspect (Proxy @a)

-- Tuple
instance Introspect (Pair k v) => Introspect (k, v) where
  field _ = field (Proxy @(Pair k v))
  introspect _ = introspect (Proxy @(Pair k v))

-- Set
instance Introspect [a] => Introspect (Set a) where
  field _ = field (Proxy @[a])
  introspect _ = introspect (Proxy @[a])

-- Map
instance Introspect (MapKind k v Maybe) => Introspect (Map k v) where
  field _ = field (Proxy @(MapKind k v Maybe))
  introspect _ = introspect (Proxy @(MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance (ObjectFields 'False a, Introspect b) => Introspect (a -> m b) where
  field _ name = (field (Proxy @b) name)
    { fieldArgs = fst $ objectFields (Proxy :: Proxy 'False) (Proxy @a)
    }
  introspect _ typeLib = resolveUpdates typeLib
                                        (introspect (Proxy @b) : argTypes)
   where
    argTypes :: [TypeUpdater]
    argTypes = snd $ objectFields (Proxy :: Proxy 'False) (Proxy @a)

-- | Introspect With specific Kind: 'kind': object, scalar, enum ...
class IntrospectKind (kind :: GQL_KIND) a where
  introspectKind :: Context kind a -> TypeUpdater -- Generates internal GraphQL Schema

isEnum :: [ConsD] -> Bool
isEnum = all isEmpty
 where
  isEmpty ConsD { cFields = [] } = True
  isEmpty _                      = False

-- SCALAR
instance (GQLType a, GQLScalar a) => IntrospectKind SCALAR a where
  introspectKind _ = updateLib scalarType [] (Proxy @a)
    where scalarType = DataScalar . buildType (scalarValidator (Proxy @a))

-- ENUM
instance (GQL_TYPE a, EnumRep (Rep a)) => IntrospectKind ENUM a where
  introspectKind _ = updateLib enumType [] (Proxy @a)
   where
    enumType =
      DataEnum . buildType (map createEnumValue $ enumTags (Proxy @(Rep a)))

-- INPUT_OBJECT
instance (GQL_TYPE a, ObjectFields (CUSTOM a) a) => IntrospectKind INPUT_OBJECT a where
  introspectKind _ = updateLib (DataInputObject . buildType fields)
                               types
                               (Proxy @a)
    where (fields, types) = objectFields (Proxy @(CUSTOM a)) (Proxy @a)

instance (GQL_TYPE a, ObjectFields (CUSTOM a) a) => IntrospectKind OBJECT a where
  introspectKind _ = updateLib (DataObject . buildType (__typename : fields))
                               types
                               (Proxy @a)
   where
    __typename =
      ( "__typename"
      , DataField { fieldName     = "__typename"
                  , fieldArgs     = []
                  , fieldArgsType = Nothing
                  , fieldType     = createAlias "String"
                  , fieldMeta     = Nothing
                  }
      )
    (fields, types) = objectFields (Proxy @(CUSTOM a)) (Proxy @a)

-- UNION
instance (GQL_TYPE a, GQLRep (Rep a)) => IntrospectKind UNION a where
  introspectKind _ = updateLib (DataUnion . buildType members) stack (Proxy @a)
   where
    unions  = concatMap cFields $ gqlRep (Proxy @(Rep a))
    stack   = map fIntro unions
    members = map fType unions


-- INPUT_UNION
instance (GQL_TYPE a, GQLRep (Rep a)) => IntrospectKind INPUT_UNION a where
  introspectKind _ = updateLib (DataInputUnion . buildType members)
                               stack
                               (Proxy @a)
   where
    unions  = concatMap cFields $ gqlRep (Proxy @(Rep a))
    stack   = map fIntro unions
    members = map fType unions

-- Types

type GQL_TYPE a = (Generic a, GQLType a)


-- Object Fields
class ObjectFields (custom :: Bool) a where
  objectFields :: proxy1 custom -> proxy2 a -> ([(Text, DataField)], [TypeUpdater])

instance GQLRep (Rep a) => ObjectFields 'False a where
  objectFields _ _ = case gqlRep (Proxy @(Rep a)) of
    [ConsD { cFields }] -> (map fFields cFields, map fIntro cFields)
    ConsD { cFields } : _ -> (map fFields cFields, map fIntro cFields)

data ConsD =  ConsD {
  cName :: Key,
  cFields :: [FieldD]
}

data FieldD = FieldD {
  fType :: Key,
  fFields :: (Text, DataField),
  fIntro :: TypeUpdater
}

--  GENERIC UNION
class GQLRep f where
  gqlRep :: Proxy f -> [ConsD]

instance GQLRep f => GQLRep (M1 D d f) where
  gqlRep _ = gqlRep (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (GQLRep a, GQLRep b) => GQLRep (a :+: b) where
  gqlRep _ = gqlRep (Proxy @a) <> gqlRep (Proxy @b)

instance (ConRep f, Constructor c) => GQLRep (M1 C c f) where
  gqlRep _ =
    [ ConsD { cName   = pack $ conName (undefined :: (M1 C c f a))
            , cFields = conRep (Proxy @f)
            }
    ]


class ConRep f where
    conRep :: Proxy f -> [FieldD]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep  a, ConRep  b) => ConRep  (a :*: b) where
  conRep _ = conRep (Proxy @a) <> conRep (Proxy @b)

instance (GQLType a, Selector s, Introspect a) => ConRep (M1 S s (Rec0 a)) where
  conRep _ =
    [ FieldD { fType   = __typeName (Proxy @a)
             , fFields = (name, field (Proxy @a) name)
             , fIntro  = introspect (Proxy @a)
             }
    ]
    where name = pack $ selName (undefined :: M1 S s (Rec0 ()) ())

instance ConRep U1 where
  conRep _ = []

buildField :: GQLType a => Proxy a -> DataArguments -> Text -> DataField
buildField proxy fieldArgs fieldName = DataField
  { fieldName
  , fieldArgs
  , fieldArgsType = Nothing
  , fieldType     = createAlias $ __typeName proxy
  , fieldMeta     = Nothing
  }

buildType :: GQLType a => t -> Proxy a -> DataTyCon t
buildType typeData proxy = DataTyCon
  { typeName        = __typeName proxy
  , typeFingerprint = __typeFingerprint proxy
  , typeMeta        = Just Meta { metaDescription = description proxy
                                , metaDirectives  = []
                                }
  , typeData
  }

updateLib
  :: GQLType a
  => (Proxy a -> DataType)
  -> [TypeUpdater]
  -> Proxy a
  -> TypeUpdater
updateLib typeBuilder stack proxy lib' =
  case isTypeDefined (__typeName proxy) lib' of
    Nothing -> resolveUpdates
      (defineType (__typeName proxy, typeBuilder proxy) lib')
      stack
    Just fingerprint' | fingerprint' == __typeFingerprint proxy -> return lib'
    -- throw error if 2 different types has same name
    Just _ -> failure $ nameCollisionError (__typeName proxy)


-- NEW AUTOMATIC DERIVATION SYSTEM

instance {-# OVERLAPPABLE #-} (GQL_TYPE a, GQLRep (Rep a)) => IntrospectKind kind a where
  introspectKind _ = builder
   where
    builder = case gqlRep Proxy @(Rep a) of
      [ConsD { cFields }] -> updateLib datatype types (Proxy @a)
       where
        datatype = DataObject . buildType (__typename : fields)
        fields   = map fFields cFields
        types    = map fIntro cFields
      cons -> updateLib dataType types (Proxy @a)
       where
        flatFields = concatMap cFields cons
        types      = map fIntro flatFields
        dataType
          | isEnum cons = DataEnum . buildType (map createEnumValue enumTags)
          | otherwise   = (DataUnion . buildType members)
         where
          enumTags = map cName cons
          members  = map fType flatFields

    -----------------------------------------------------------------------------  
    __typename =
      ( "__typename"
      , DataField { fieldName     = "__typename"
                  , fieldArgs     = []
                  , fieldArgsType = Nothing
                  , fieldType     = createAlias "String"
                  , fieldMeta     = Nothing
                  }
      )

