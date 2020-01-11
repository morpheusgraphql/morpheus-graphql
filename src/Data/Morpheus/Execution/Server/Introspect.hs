{-# LANGUAGE TupleSections #-}
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
  , IntrospectRep(..)
  , IntroCon
  , updateLib
  , buildType
  , introspectObjectFields
  , TypeScope(..)
  )
where

import           Data.Map                       ( Map )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics
import           Data.Semigroup                 ( (<>) )
import           Data.List                      ( partition )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( globalErrorMessage )
import           Data.Morpheus.Error.Schema     ( nameCollisionError )
import           Data.Morpheus.Execution.Server.Generics.EnumRep
                                                ( EnumRep(..) )
import           Data.Morpheus.Kind             ( Context(..)
                                                , ENUM
                                                , GQL_KIND
                                                , SCALAR
                                                , OUTPUT
                                                , INPUT
                                                )
import           Data.Morpheus.Types.Types      ( MapKind
                                                , Pair
                                                )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar(..) )
import           Data.Morpheus.Types.GQLType    ( GQLType(..) )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..)
                                                , resolveUpdates
                                                , Resolver
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , ArgumentsDefinition(..)
                                                , Meta(..)
                                                , FieldDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , Key
                                                , createAlias
                                                , defineType
                                                , isTypeDefined
                                                , toListField
                                                , toNullableField
                                                , createEnumValue
                                                , TypeUpdater
                                                , DataFingerprint(..)
                                                , DataUnion
                                                , FieldsDefinition(..)
                                                , TypeRef(..)
                                                , Message
                                                , Listable(..)
                                                )

type IntroCon a = (GQLType a, IntrospectRep (CUSTOM a) a)


-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class Introspect a where
  isObject :: proxy a -> Bool
  default isObject :: GQLType a => proxy a -> Bool
  isObject _ = isObjectKind (Proxy @a)
  field :: proxy a -> Text -> FieldDefinition
  introspect :: proxy a -> TypeUpdater
  -----------------------------------------------
  default field :: GQLType a =>
    proxy a -> Text -> FieldDefinition
  field _ = buildField (Proxy @a) NoArguments

instance {-# OVERLAPPABLE #-} (GQLType a, IntrospectKind (KIND a) a) => Introspect a where
  introspect _ = introspectKind (Context :: Context (KIND a) a)

-- Maybe
instance Introspect a => Introspect (Maybe a) where
  isObject _ = False
  field _ = toNullableField . field (Proxy @a)
  introspect _ = introspect (Proxy @a)

-- List
instance Introspect a => Introspect [a] where
  isObject _ = False
  field _ = toListField . field (Proxy @a)
  introspect _ = introspect (Proxy @a)

-- Tuple
instance Introspect (Pair k v) => Introspect (k, v) where
  isObject _ = True
  field _ = field (Proxy @(Pair k v))
  introspect _ = introspect (Proxy @(Pair k v))

-- Set
instance Introspect [a] => Introspect (Set a) where
  isObject _ = False
  field _ = field (Proxy @[a])
  introspect _ = introspect (Proxy @[a])

-- Map
instance Introspect (MapKind k v Maybe) => Introspect (Map k v) where
  isObject _ = True
  field _ = field (Proxy @(MapKind k v Maybe))
  introspect _ = introspect (Proxy @(MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance (GQLType b, IntrospectRep 'False a, Introspect b) => Introspect (a -> m b) where
  isObject _ = False
  field _ name = fieldObj { fieldArgs }
   where
    fieldObj  = field (Proxy @b) name
    fieldArgs = ArgumentsDefinition Nothing $ toList $ fst  $ introspectObjectFields
      (Proxy :: Proxy 'False)
      (__typeName (Proxy @b), OutputType, Proxy @a)
  introspect _ typeLib = resolveUpdates typeLib
                                        (introspect (Proxy @b) : inputs)
   where
    name = "Arguments for " <> __typeName (Proxy @b)
    inputs :: [TypeUpdater]
    inputs =
      snd $ introspectObjectFields (Proxy :: Proxy 'False) (name, InputType, Proxy @a)

--  GQL Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (GQLType b, Introspect b) => Introspect (Resolver fo e m b) where
  isObject _ = False
  field _ = field (Proxy @b)
  introspect _ = introspect (Proxy @b)


-- | Introspect With specific Kind: 'kind': object, scalar, enum ...
class IntrospectKind (kind :: GQL_KIND) a where
  introspectKind :: Context kind a -> TypeUpdater -- Generates internal GraphQL Schema

-- SCALAR
instance (GQLType a, GQLScalar a) => IntrospectKind SCALAR a where
  introspectKind _ = updateLib scalarType [] (Proxy @a)
    where scalarType = buildType $ DataScalar $ scalarValidator (Proxy @a)

-- ENUM
instance (GQL_TYPE a, EnumRep (Rep a)) => IntrospectKind ENUM a where
  introspectKind _ = updateLib enumType [] (Proxy @a)
   where
    enumType =
      buildType $ DataEnum $ map createEnumValue $ enumTags (Proxy @(Rep a))

instance (GQL_TYPE a, IntrospectRep (CUSTOM a) a) => IntrospectKind INPUT a where
  introspectKind _ = derivingData (Proxy @a) InputType

instance (GQL_TYPE a, IntrospectRep (CUSTOM a) a) => IntrospectKind OUTPUT a where
  introspectKind _ = derivingData (Proxy @a) OutputType

derivingData
  :: forall a
   . (GQLType a, IntrospectRep (CUSTOM a) a)
  => Proxy a
  -> TypeScope
  -> TypeUpdater
derivingData _ scope = updateLib (buildType datatypeContent) updates (Proxy @a)
 where
  (datatypeContent, updates) = introspectRep
    (Proxy @(CUSTOM a))
    (Proxy @a, scope, baseName, baseFingerprint)
  baseName        = __typeName (Proxy @a)
  baseFingerprint = __typeFingerprint (Proxy @a)

type GQL_TYPE a = (Generic a, GQLType a)

introspectObjectFields
  :: IntrospectRep custom a
  => proxy1 (custom :: Bool)
  -> (Name, TypeScope, proxy2 a)
  -> (FieldsDefinition, [TypeUpdater])
introspectObjectFields p1 (name, scope, proxy) = withObject
  (introspectRep p1 (proxy, scope, "", DataFingerprint "" []))
 where
  withObject (DataObject     {objectFields}, ts) = (objectFields, ts)
  withObject (DataInputObject x, ts) = (x, ts)
  withObject _ =
    ( fromList ([] :: [FieldDefinition]) , [introspectFailure (name <> " should have only one nonempty constructor")])

introspectFailure :: Message -> TypeUpdater
introspectFailure = const . failure . globalErrorMessage . ("invalid schema: " <>)

-- Object Fields
class IntrospectRep (custom :: Bool) a where
  introspectRep :: proxy1 custom -> ( proxy2 a,TypeScope,Name,DataFingerprint) -> (TypeContent, [TypeUpdater])

instance (TypeRep (Rep a) , Generic a) => IntrospectRep 'False a where
  introspectRep _ (_, scope, name, fing) =
    derivingDataContent (Proxy @a) (name, fing) scope

buildField :: GQLType a => Proxy a -> ArgumentsDefinition -> Text -> FieldDefinition
buildField proxy fieldArgs fieldName = FieldDefinition
  { fieldName
  , fieldArgs
  , fieldType     = createAlias $ __typeName proxy
  , fieldMeta     = Nothing
  }

buildType :: GQLType a => TypeContent -> Proxy a -> TypeDefinition
buildType typeContent proxy = TypeDefinition
  { typeName        = __typeName proxy
  , typeFingerprint = __typeFingerprint proxy
  , typeMeta        = Just Meta { metaDescription = description proxy
                                , metaDirectives  = []
                                }
  , typeContent
  }

updateLib
  :: GQLType a
  => (Proxy a -> TypeDefinition)
  -> [TypeUpdater]
  -> Proxy a
  -> TypeUpdater
updateLib typeBuilder stack proxy lib =
  case isTypeDefined (__typeName proxy) lib of
    Nothing -> resolveUpdates
      (defineType (typeBuilder proxy) lib)
      stack
    Just fingerprint' | fingerprint' == __typeFingerprint proxy -> return lib
    -- throw error if 2 different types has same name
    Just _ -> failure $ nameCollisionError (__typeName proxy)


-- NEW AUTOMATIC DERIVATION SYSTEM

data ConsRep = ConsRep {
  consName :: Key,
  consIsRecord :: Bool,
  consFields :: [FieldRep]
}

data FieldRep = FieldRep {
  fieldTypeName :: Name,
  fieldData :: FieldDefinition,
  fieldTypeUpdater :: TypeUpdater,
  fieldIsObject :: Bool
}

data ResRep = ResRep {
  enumCons :: [Name],
  unionRef :: [Name],
  unionRecordRep :: [ConsRep ]
}

isEmpty :: ConsRep -> Bool
isEmpty ConsRep { consFields = [] } = True
isEmpty _                           = False

isUnionRecord :: ConsRep -> Bool
isUnionRecord ConsRep { consIsRecord } = consIsRecord

isUnionRef :: Name -> ConsRep  -> Bool
isUnionRef baseName ConsRep { consName, consFields = [FieldRep { fieldIsObject = True, fieldTypeName }] }
  = consName == baseName <> fieldTypeName
isUnionRef _ _ = False

setFieldNames :: ConsRep -> ConsRep
setFieldNames cons@ConsRep { consFields } = cons
  { consFields = zipWith setFieldName ([0 ..] :: [Int]) consFields
  }
 where
  setFieldName i fieldR@FieldRep { fieldData = fieldD } = fieldR { fieldData = fieldD { fieldName } }
    where fieldName = "_" <> pack (show i)

analyseRep :: Name -> [ConsRep] -> ResRep
analyseRep baseName cons = ResRep
  { enumCons       = map consName enumRep
  , unionRef       = map fieldTypeName $ concatMap consFields unionRefRep
  , unionRecordRep = unionRecordRep <> map setFieldNames anyonimousUnionRep
  }
 where
  (enumRep       , left1             ) = partition isEmpty cons
  (unionRefRep   , left2             ) = partition (isUnionRef baseName) left1
  (unionRecordRep, anyonimousUnionRep) = partition isUnionRecord left2

derivingDataContent
  :: forall a
   . (Generic a, TypeRep (Rep a))
  => Proxy a
  -> (Name, DataFingerprint)
  -> TypeScope
  -> (TypeContent, [TypeUpdater])
derivingDataContent _ (baseName, baseFingerprint) scope =
  builder $ typeRep $ Proxy @(Rep a)
 where
  builder [ConsRep { consFields }] = buildObject scope consFields
  builder cons                     = genericUnion scope cons
   where
    genericUnion InputType = buildInputUnion (baseName, baseFingerprint)
    genericUnion OutputType =
      buildUnionType (baseName, baseFingerprint) DataUnion (DataObject [])


buildInputUnion
  :: (Name, DataFingerprint) -> [ConsRep ] -> (TypeContent, [TypeUpdater])
buildInputUnion (baseName, baseFingerprint) cons = datatype
  (analyseRep baseName cons)
 where
  datatype ResRep { unionRef = [], unionRecordRep = [], enumCons } =
    (DataEnum (map createEnumValue enumCons), types)
  datatype ResRep { unionRef, unionRecordRep, enumCons } =
    (DataInputUnion typeMembers, types <> unionTypes)
   where
    typeMembers =
      map (, True) (unionRef <> unionMembers) <> map (, False) enumCons
    (unionMembers, unionTypes) =
      buildUnions DataInputObject baseFingerprint unionRecordRep
  types = map fieldTypeUpdater $ concatMap consFields cons

buildUnionType
  :: (Name, DataFingerprint)
  -> (DataUnion -> TypeContent)
  -> (FieldsDefinition -> TypeContent)
  -> [ConsRep]
  -> (TypeContent, [TypeUpdater])
buildUnionType (baseName, baseFingerprint) wrapUnion wrapObject cons = datatype
  (analyseRep baseName cons)
 where
  datatype ResRep { unionRef = [], unionRecordRep = [], enumCons } =
    (DataEnum (map createEnumValue enumCons), types)
  datatype ResRep { unionRef, unionRecordRep, enumCons } =
    (wrapUnion typeMembers, types <> enumTypes <> unionTypes)
   where
    typeMembers = unionRef <> enumMembers <> unionMembers
    (enumMembers, enumTypes) =
      buildUnionEnum wrapObject baseName baseFingerprint enumCons
    (unionMembers, unionTypes) =
      buildUnions wrapObject baseFingerprint unionRecordRep
  types = map fieldTypeUpdater $ concatMap consFields cons


buildObject :: TypeScope -> [FieldRep] -> (TypeContent, [TypeUpdater])
buildObject isOutput consFields = (wrapWith fields, types)
 where
  (fields, types) = buildDataObject consFields
  wrapWith | isOutput == OutputType = DataObject [] 
           | otherwise              = DataInputObject 

buildDataObject :: [FieldRep] -> (FieldsDefinition , [TypeUpdater])
buildDataObject consFields = (fields, types)
 where
  fields = fromList $ map fieldData consFields
  types  = map fieldTypeUpdater consFields

buildUnions
  :: (FieldsDefinition -> TypeContent)
  -> DataFingerprint
  -> [ConsRep]
  -> ([Name], [TypeUpdater])
buildUnions wrapObject baseFingerprint cons = (members, map buildURecType cons)
 where
  buildURecType consRep = pure . defineType
      (buildUnionRecord wrapObject baseFingerprint consRep)
  members = map consName cons

buildUnionRecord
  :: (FieldsDefinition -> TypeContent) -> DataFingerprint -> ConsRep -> TypeDefinition
buildUnionRecord wrapObject typeFingerprint ConsRep { consName, consFields } = TypeDefinition 
    { typeName        = consName
    , typeFingerprint
    , typeMeta        = Nothing
    , typeContent     = wrapObject $ fromList $ map fieldData consFields
    }

buildUnionEnum
  :: (FieldsDefinition -> TypeContent)
  -> Name
  -> DataFingerprint
  -> [Name]
  -> ([Name], [TypeUpdater])
buildUnionEnum wrapObject baseName baseFingerprint enums = (members, updates)
 where
  members | null enums = []
          | otherwise  = [enumTypeWrapperName]
  enumTypeName        = baseName <> "Enum"
  enumTypeWrapperName = enumTypeName <> "Object"
  -------------------------
  updates :: [TypeUpdater]
  updates
    | null enums
    = []
    | otherwise
    = [ buildEnumObject wrapObject
                        enumTypeWrapperName
                        baseFingerprint
                        enumTypeName
      , buildEnum enumTypeName baseFingerprint enums
      ]

buildEnum :: Name -> DataFingerprint -> [Name] -> TypeUpdater
buildEnum typeName typeFingerprint tags = pure . defineType
  TypeDefinition { typeName
             , typeFingerprint
             , typeMeta        = Nothing
             , typeContent     = DataEnum $ map createEnumValue tags
           }

buildEnumObject
  :: (FieldsDefinition -> TypeContent)
  -> Name
  -> DataFingerprint
  -> Name
  -> TypeUpdater
buildEnumObject wrapObject typeName typeFingerprint enumTypeName =
  pure . defineType
    TypeDefinition
      { typeName
      , typeFingerprint
      , typeMeta        = Nothing
      , typeContent     = wrapObject $ fromList 
          [ FieldDefinition 
            { fieldName  = "enum"
            , fieldArgs  = NoArguments
            , fieldType  = createAlias enumTypeName
            , fieldMeta  = Nothing
            }
          ]
      }

data TypeScope = InputType | OutputType deriving (Show,Eq,Ord)

--  GENERIC UNION
class TypeRep f where
  typeRep :: Proxy f -> [ConsRep]

instance TypeRep f => TypeRep (M1 D d f) where
  typeRep _ = typeRep (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep a, TypeRep b) => TypeRep (a :+: b) where
  typeRep _ = typeRep (Proxy @a) <> typeRep (Proxy @b)

instance (ConRep f, Constructor c) => TypeRep (M1 C c f) where
  typeRep _ =
    [ ConsRep { consName     = pack $ conName (undefined :: (M1 C c f a))
              , consFields   = conRep (Proxy @f)
              , consIsRecord = conIsRecord (undefined :: (M1 C c f a))
              }
    ]

class ConRep f where
    conRep :: Proxy f -> [FieldRep]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep  a, ConRep  b) => ConRep  (a :*: b) where
  conRep _ = conRep (Proxy @a) <> conRep (Proxy @b)

instance (Selector s, Introspect a) => ConRep (M1 S s (Rec0 a)) where
  conRep _ =
    [ FieldRep { fieldTypeName    = typeConName $ fieldType fieldData
               , fieldData        = fieldData
               , fieldTypeUpdater = introspect (Proxy @a)
               , fieldIsObject    = isObject (Proxy @a)
               }
    ]
   where
    name      = pack $ selName (undefined :: M1 S s (Rec0 ()) ())
    fieldData = field (Proxy @a) name

instance ConRep U1 where
  conRep _ = []
