{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving  , TemplateHaskell   #-}

module Data.Morpheus.Types.Internal.AST.Data
  ( DataScalar
  , DataEnum
  , FieldsDefinition(..)
  , DataArgument
  , DataUnion
  , ArgumentsDefinition(..)
  , FieldDefinition(..)
  , DataTypeContent(..)
  , DataType(..)
  , Schema(..)
  , DataTypeWrapper(..)
  , DataValidator(..)
  , DataTypeKind(..)
  , DataFingerprint(..)
  , TypeWrapper(..)
  , TypeRef(..)
  , DataEnumValue(..)
  , isTypeDefined
  , initTypeLib
  , defineType
  , isFieldNullable
  , allDataTypes
  , lookupDataType
  , kindOf
  , toNullableField
  , toListField
  , isObject
  , isInput
  , toHSWrappers
  , isNullable
  , toGQLWrapper
  , isWeaker
  , isSubscription
  , isOutputObject
  , sysTypes
  , isDefaultTypeName
  , isSchemaTypeName
  , isPrimitiveTypeName
  , OperationType(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , isEntNode
  , lookupInputType
  , coerceDataObject
  , lookupDataUnion
  , lookupField
  , lookupUnionTypes
  , lookupSelectionField
  , lookupFieldAsSelectionSet
  , createField
  , createArgument
  , createDataTypeLib
  , createEnumType
  , createScalarType
  , createType
  , createUnionType
  , createAlias
  , createInputUnionFields
  , fieldVisibility
  , Meta(..)
  , Directive(..)
  , createEnumValue
  , insertType
  , TypeUpdater
  , lookupDeprecated
  , lookupDeprecatedReason
  , TypeD(..)
  , ConsD(..)
  , ClientQuery(..)
  , GQLTypeD(..)
  , ClientType(..)
  , DataInputUnion
  , isNullableWrapper
  , isOutputType
  , checkForUnknownKeys
  , checkNameCollision
  , Collectible(..)
  , hasArguments
  )
where

import           Data.HashMap.Lazy              ( HashMap
                                                , empty
                                                , fromList
                                                , insert
                                                , toList
                                                , union
                                                , elems
                                                )
import qualified Data.HashMap.Lazy             as HM
import           Data.Semigroup                 ( (<>) )
import           Language.Haskell.TH.Syntax     ( Lift(..) )
import           Instances.TH.Lift              ( )
import           Data.List                      ( find , (\\))

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalError )
import           Data.Morpheus.Error.Selection  ( cannotQueryField
                                                , hasNoSubfields
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key
                                                , Position
                                                , Name
                                                , Description
                                                , TypeWrapper(..)
                                                , TypeRef(..)
                                                , Ref(..)
                                                , elementOfKeys
                                                , removeDuplicates
                                                )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Validation
                                                , Failure(..)
                                                , GQLErrors
                                                , LibUpdater
                                                , resolveUpdates
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value(..) 
                                                , ValidValue
                                                , ScalarValue(..)
                                                )
import           Data.Morpheus.Error.Schema     ( nameCollisionError )


class Collectible c a where 
  wrap     :: [(Name, a)] ->  c
  unwrap   ::  c  -> [(Name, a)]
  selectBy :: (Failure e m, Monad m) => e -> Name -> c -> m a 
  

type QUERY = 'Query
type MUTATION = 'Mutation
type SUBSCRIPTION = 'Subscription

isDefaultTypeName :: Key -> Bool
isDefaultTypeName x = isSchemaTypeName x || isPrimitiveTypeName x

isSchemaTypeName :: Key -> Bool
isSchemaTypeName = (`elem` sysTypes)

isPrimitiveTypeName :: Key -> Bool
isPrimitiveTypeName = (`elem` ["String", "Float", "Int", "Boolean", "ID"])


checkNameCollision :: (Failure e m, Ord a) => [a] -> ([a] -> e) -> m [a]
checkNameCollision enhancedKeys errorGenerator =
  case enhancedKeys \\ removeDuplicates enhancedKeys of
    []         -> pure enhancedKeys
    duplicates -> failure $ errorGenerator duplicates

checkForUnknownKeys :: Failure e m => [Ref] -> [Name] -> ([Ref] -> e) -> m [Ref]
checkForUnknownKeys enhancedKeys' keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys' of
    []           -> pure enhancedKeys'
    unknownKeys' -> failure $ errorGenerator' unknownKeys'  


sysTypes :: [Key]
sysTypes =
  [ "__Schema"
  , "__Type"
  , "__Directive"
  , "__TypeKind"
  , "__Field"
  , "__DirectiveLocation"
  , "__InputValue"
  , "__EnumValue"
  ]

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Lift)

isSubscription :: DataTypeKind -> Bool
isSubscription (KindObject (Just Subscription)) = True
isSubscription _ = False

isOutputType :: DataTypeKind -> Bool
isOutputType (KindObject _) = True
isOutputType KindUnion      = True
isOutputType _              = False

isOutputObject :: DataTypeKind -> Bool
isOutputObject (KindObject _) = True
isOutputObject _              = False

isObject :: DataTypeKind -> Bool
isObject (KindObject _)  = True
isObject KindInputObject = True
isObject _               = False

isInput :: DataTypeKind -> Bool
isInput KindInputObject = True
isInput _               = False

data DataTypeKind
  = KindScalar
  | KindObject (Maybe OperationType)
  | KindUnion
  | KindEnum
  | KindInputObject
  | KindList
  | KindNonNull
  | KindInputUnion
  deriving (Eq, Show, Lift)

isFieldNullable :: FieldDefinition -> Bool
isFieldNullable = isNullable . fieldType

isNullable :: TypeRef -> Bool
isNullable TypeRef { typeWrappers = typeWrappers } = isNullableWrapper typeWrappers

isNullableWrapper :: [TypeWrapper] -> Bool
isNullableWrapper (TypeMaybe : _ ) = True
isNullableWrapper _               = False


isWeaker :: [TypeWrapper] -> [TypeWrapper] -> Bool
isWeaker (TypeMaybe : xs1) (TypeMaybe : xs2) = isWeaker xs1 xs2
isWeaker (TypeMaybe : _  ) _                 = True
isWeaker (_         : xs1) (_ : xs2)         = isWeaker xs1 xs2
isWeaker _                 _                 = False

toGQLWrapper :: [TypeWrapper] -> [DataTypeWrapper]
toGQLWrapper (TypeMaybe : (TypeMaybe : tw)) = toGQLWrapper (TypeMaybe : tw)
toGQLWrapper (TypeMaybe : (TypeList  : tw)) = ListType : toGQLWrapper tw
toGQLWrapper (TypeList : tw) = [NonNullType, ListType] <> toGQLWrapper tw
toGQLWrapper [TypeMaybe                   ] = []
toGQLWrapper []                             = [NonNullType]

toHSWrappers :: [DataTypeWrapper] -> [TypeWrapper]
toHSWrappers (NonNullType : (NonNullType : xs)) =
  toHSWrappers (NonNullType : xs)
toHSWrappers (NonNullType : (ListType : xs)) = TypeList : toHSWrappers xs
toHSWrappers (ListType : xs) = [TypeMaybe, TypeList] <> toHSWrappers xs
toHSWrappers []                              = [TypeMaybe]
toHSWrappers [NonNullType]                   = []

data DataFingerprint = DataFingerprint Name [String] deriving (Show, Eq, Ord, Lift)

newtype DataValidator = DataValidator
  { validateValue :: ValidValue -> Either Key ValidValue
  }

instance Show DataValidator where
  show _ = "DataValidator"

type DataScalar = DataValidator
type DataEnum = [DataEnumValue]
type DataArgument = FieldDefinition 
type DataUnion = [Key]
type DataInputUnion = [(Key, Bool)]

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show, Lift)

data Directive = Directive {
  directiveName :: Name,
  directiveArgs :: [(Name, ValidValue)]
} deriving (Show,Lift)

-- META
data Meta = Meta {
    metaDescription:: Maybe Description,
    metaDirectives  :: [Directive]
} deriving (Show,Lift)

lookupDeprecated :: Meta -> Maybe Directive
lookupDeprecated Meta { metaDirectives } = find isDeprecation metaDirectives
 where
  isDeprecation Directive { directiveName = "deprecated" } = True
  isDeprecation _ = False

lookupDeprecatedReason :: Directive -> Maybe Key
lookupDeprecatedReason Directive { directiveArgs } =
  maybeString . snd <$> find isReason directiveArgs
 where
  maybeString :: ValidValue -> Name
  maybeString (Scalar (String x)) = x
  maybeString _                   = "can't read deprecated Reason Value"
  isReason ("reason", _) = True
  isReason _             = False

-- ENUM VALUE
data DataEnumValue = DataEnumValue{
    enumName :: Name,
    enumMeta :: Maybe Meta
} deriving (Show, Lift)

-- 3.2 Schema : https://graphql.github.io/graphql-spec/June2018/#sec-Schema
---------------------------------------------------------------------------
-- SchemaDefinition :
--    schema Directives[Const](opt) { RootOperationTypeDefinition(list)}
--
-- RootOperationTypeDefinition :
--    OperationType: NamedType

data Schema = Schema
  { types        :: HashMap Name DataType
  , query        :: DataType
  , mutation     :: Maybe DataType
  , subscription :: Maybe DataType
  } deriving (Show)

type TypeRegister = HashMap Key DataType

initTypeLib :: DataType -> Schema
initTypeLib query = Schema { types        = empty
                             , query        = query
                             , mutation     = Nothing
                             , subscription = Nothing
                            }

allDataTypes :: Schema -> [DataType]
allDataTypes  = elems . typeRegister

typeRegister :: Schema -> TypeRegister
typeRegister Schema { types, query, mutation, subscription } =
  types `union` fromList
    (concatMap fromOperation [Just query, mutation, subscription])

createDataTypeLib :: [(Key, DataType)] -> Validation Schema
createDataTypeLib types = case popByKey "Query" types of
  (Nothing   ,_    ) -> internalError "Query Not Defined"
  (Just query, lib1) -> do
    let (mutation, lib2) = popByKey "Mutation" lib1
    let (subscription, lib3) = popByKey "Subscription" lib2
    pure $ (foldr defineType (initTypeLib query) lib3) {mutation, subscription}


-- 3.4 Types : https://graphql.github.io/graphql-spec/June2018/#sec-Types
-------------------------------------------------------------------------
-- TypeDefinition :
--   ScalarTypeDefinition
--   ObjectTypeDefinition
--   InterfaceTypeDefinition
--   UnionTypeDefinition
--   EnumTypeDefinition
--   InputObjectTypeDefinition

data DataType = DataType
  { typeName        :: Key
  , typeFingerprint :: DataFingerprint
  , typeMeta        :: Maybe Meta
  , typeContent     :: DataTypeContent
  } deriving (Show)

data DataTypeContent
  = DataScalar      { dataScalar        :: DataScalar   }
  | DataEnum        { enumMembers       :: DataEnum     }
  | DataInputObject { inputObjectFields :: FieldsDefinition   }
  | DataObject      { objectImplements  :: [Name],
                      objectFields      :: FieldsDefinition   }
  | DataUnion       { unionMembers      :: DataUnion    }
  | DataInputUnion  { inputUnionMembers :: [(Key,Bool)] }
  | DataInterface   { interfaceFields   :: FieldsDefinition    }
  deriving (Show)

createType :: Key -> DataTypeContent -> DataType
createType typeName typeContent = DataType
  { typeName
  , typeMeta        = Nothing
  , typeFingerprint = DataFingerprint typeName []
  , typeContent
  }

createScalarType :: Name -> DataType
createScalarType typeName = createType typeName $ DataScalar (DataValidator pure)

createEnumType :: Name -> [Key] -> DataType
createEnumType typeName typeData = createType typeName (DataEnum enumValues)
  where enumValues = map createEnumValue typeData

createEnumValue :: Name -> DataEnumValue
createEnumValue enumName = DataEnumValue { enumName, enumMeta = Nothing }

createUnionType :: Key -> [Key] -> DataType
createUnionType typeName typeData = createType typeName (DataUnion typeData)

isEntNode :: DataTypeContent -> Bool
isEntNode DataScalar{}  = True
isEntNode DataEnum{} = True
isEntNode _ = False

isInputDataType :: DataType -> Bool
isInputDataType DataType { typeContent } = __isInput typeContent
 where
  __isInput DataScalar{}      = True
  __isInput DataEnum{}        = True
  __isInput DataInputObject{} = True
  __isInput DataInputUnion{}  = True
  __isInput _                 = False

coerceDataObject :: Failure error m => error -> DataType -> m (Name, FieldsDefinition)
coerceDataObject _ DataType { typeContent = DataObject { objectFields } , typeName } = pure (typeName, objectFields)
coerceDataObject gqlError _ = failure gqlError

coerceDataUnion :: Failure error m => error -> DataType -> m DataUnion
coerceDataUnion _ DataType { typeContent = DataUnion members } = pure members
coerceDataUnion gqlError _ = failure gqlError

kindOf :: DataType -> DataTypeKind
kindOf DataType { typeContent } = __kind typeContent
 where
  __kind DataScalar      {} = KindScalar
  __kind DataEnum        {} = KindEnum
  __kind DataInputObject {} = KindInputObject
  __kind DataObject      {} = KindObject Nothing
  __kind DataUnion       {} = KindUnion
  __kind DataInputUnion  {} = KindInputUnion

fromOperation :: Maybe DataType -> [(Name, DataType)]
fromOperation (Just datatype) = [(typeName datatype,datatype)]
fromOperation Nothing = []

lookupUnionTypes
  :: (Monad m, Failure GQLErrors m)
  => Position
  -> Key
  -> Schema
  -> FieldDefinition 
  -> m [(Name, FieldsDefinition)]
lookupUnionTypes position key lib FieldDefinition { fieldType = TypeRef { typeConName = typeName } }
  = lookupDataUnion gqlError typeName lib
    >>= mapM (flip (selectBy gqlError) lib)
  where gqlError = hasNoSubfields key typeName position

lookupDataUnion
  :: (Monad m, Failure e m) => e -> Key -> Schema -> m DataUnion
lookupDataUnion validationError name lib =
  selectBy validationError name lib >>= coerceDataUnion validationError

lookupDataType :: Key -> Schema -> Maybe DataType
lookupDataType name  = HM.lookup name . typeRegister

lookupInputType :: Failure e m => Key -> Schema -> e -> m DataType
lookupInputType name lib errors = case lookupDataType name lib of
  Just x | isInputDataType x -> pure x
  _                          -> failure errors

isTypeDefined :: Key -> Schema -> Maybe DataFingerprint
isTypeDefined name lib = typeFingerprint <$> lookupDataType name lib

defineType :: (Key, DataType) -> Schema -> Schema
defineType (key, datatype@DataType { typeName, typeContent = DataInputUnion enumKeys, typeFingerprint }) lib
  = lib { types = insert name unionTags (insert key datatype (types lib)) }
 where
  name      = typeName <> "Tags"
  unionTags = DataType
    { typeName        = name
    , typeFingerprint
    , typeMeta        = Nothing
    , typeContent     = DataEnum $ map (createEnumValue . fst) enumKeys
    }
defineType (key, datatype) lib =
  lib { types = insert key datatype (types lib) }

insertType :: (Key, DataType) -> TypeUpdater
insertType nextType@(name, datatype) lib = case isTypeDefined name lib of
  Nothing -> resolveUpdates (defineType nextType lib) []
  Just fingerprint | fingerprint == typeFingerprint datatype -> return lib
                   |
      -- throw error if 2 different types has same name
                     otherwise -> failure $ nameCollisionError name

-- lookups and removes DataType from hashmap 
popByKey :: Name -> [(Key, DataType)] -> (Maybe DataType,[(Key, DataType)])
popByKey key lib = case lookup key lib of
    Just dt@DataType { typeContent = DataObject {} } ->
      (Just dt, filter ((/= key) . fst) lib)
    _ -> (Nothing, lib)  

instance Collectible Schema DataType where 
  selectBy err name lib = case lookupDataType name lib of
      Nothing -> failure err
      Just x  -> pure x

-- 3.6 Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
------------------------------------------------------------------------------
--  ObjectTypeDefinition:
--    Description(opt) type Name ImplementsInterfaces(opt) Directives(Const)(opt) FieldsDefinition(opt)
--
--  ImplementsInterfaces
--    implements &(opt) NamedType
--    ImplementsInterfaces & NamedType
--
--  FieldsDefinition
--    { FieldDefinition(list) }
--
newtype FieldsDefinition = FieldsDefinition 
  { unFieldsDefinition :: HashMap Name FieldDefinition } deriving (Show)

instance Lift FieldsDefinition where 
  lift (FieldsDefinition  hm) = [| FieldsDefinition $ HM.fromList ls |]
    where ls = HM.toList hm

instance Semigroup FieldsDefinition where 
  FieldsDefinition x <> FieldsDefinition y = FieldsDefinition (x <> y)

instance Collectible FieldsDefinition FieldDefinition where
  wrap = FieldsDefinition . HM.fromList 
  unwrap = HM.toList . unFieldsDefinition
  selectBy err name (FieldsDefinition lib) = case HM.lookup name lib of
      Nothing -> failure err
      Just x  -> pure x

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
-- 
data FieldDefinition = FieldDefinition
  { fieldName     :: Key
  , fieldArgs     :: ArgumentsDefinition
  , fieldType     :: TypeRef
  , fieldMeta     :: Maybe Meta
  } deriving (Show,Lift)

fieldVisibility :: (Key, FieldDefinition) -> Bool
fieldVisibility ("__typename", _) = False
fieldVisibility ("__schema"  , _) = False
fieldVisibility ("__type"    , _) = False
fieldVisibility _                 = True

createField :: ArgumentsDefinition -> Key -> ([TypeWrapper], Key) -> FieldDefinition
createField dataArguments fieldName (typeWrappers, typeConName) = FieldDefinition
  { fieldArgs = dataArguments
  , fieldName
  , fieldType     = TypeRef { typeConName, typeWrappers, typeArgs = Nothing }
  , fieldMeta     = Nothing
  }

toNullableField :: FieldDefinition -> FieldDefinition
toNullableField dataField
  | isNullable (fieldType dataField) = dataField
  | otherwise = dataField { fieldType = nullable (fieldType dataField) }
 where
  nullable alias@TypeRef { typeWrappers } =
    alias { typeWrappers = TypeMaybe : typeWrappers }

toListField :: FieldDefinition -> FieldDefinition
toListField dataField = dataField { fieldType = listW (fieldType dataField) }
 where
  listW alias@TypeRef { typeWrappers } =
    alias { typeWrappers = TypeList : typeWrappers }


instance Collectible Schema (Name, FieldsDefinition ) where 
  selectBy validationError name lib =
     selectBy validationError name lib >>= coerceDataObject validationError

lookupField :: Failure error m => Key -> [(Key, field)] -> error -> m field
lookupField key fields gqlError = case lookup key fields of
  Nothing    -> failure gqlError
  Just field -> pure field

lookupSelectionField
  :: Failure GQLErrors Validation
  => Position
  -> Name
  -> Name
  -> FieldsDefinition
  -> Validation (FieldDefinition)
lookupSelectionField position fieldName typeName fields = selectBy gqlError fieldName fields 
  where gqlError = cannotQueryField fieldName typeName position

lookupFieldAsSelectionSet
  :: (Monad m, Failure GQLErrors m)
  => Position
  -> Key
  -> Schema
  -> FieldDefinition  
  -> m (Name, FieldsDefinition )
lookupFieldAsSelectionSet position key lib FieldDefinition { fieldType = TypeRef { typeConName } }
  = selectBy gqlError typeConName lib
  where gqlError = hasNoSubfields key typeConName position

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

data ArgumentsDefinition 
  = ArgumentsDefinition  
    { argumentsTypename ::  Maybe Name
    , arguments         :: [(Key, DataArgument)] 
    }
  | NoArguments
  deriving (Show, Lift)

createArgument :: Key -> ([TypeWrapper], Key) -> (Key, FieldDefinition)
createArgument fieldName x = (fieldName, createField NoArguments fieldName x)

hasArguments :: ArgumentsDefinition -> Bool
hasArguments NoArguments = False
hasArguments _ = True

-- InputValueDefinition
--   Description(opt) Name: TypeDefaultValue(opt) Directives[Const](opt)

createInputUnionFields :: Key -> [Key] -> [(Key, FieldDefinition)]
createInputUnionFields name members = fieldTag : map unionField members
 where
  fieldTag =
    ( "__typename"
    , FieldDefinition { fieldName     = "__typename"
                , fieldArgs     = NoArguments
                , fieldType     = createAlias (name <> "Tags")
                , fieldMeta     = Nothing
                }
    )
  unionField memberName =
    ( memberName
    , FieldDefinition
      { fieldArgs     = NoArguments
      , fieldName     = memberName
      , fieldType     = TypeRef { typeConName    = memberName
                                  , typeWrappers = [TypeMaybe]
                                  , typeArgs     = Nothing
                                  }
      , fieldMeta     = Nothing
      }
    )

--
-- OTHER
--------------------------------------------------------------------------------------------------

createAlias :: Key -> TypeRef
createAlias typeConName =
  TypeRef { typeConName, typeWrappers = [], typeArgs = Nothing }

type TypeUpdater = LibUpdater Schema

-- TEMPLATE HASKELL DATA TYPES
data ClientQuery = ClientQuery
  { queryText     :: String
  , queryTypes    :: [ClientType]
  , queryArgsType :: Maybe TypeD
  } deriving (Show)

data ClientType = ClientType {
  clientType :: TypeD,
  clientKind :: DataTypeKind
} deriving (Show)

-- Document
data GQLTypeD = GQLTypeD
  { typeD     :: TypeD
  , typeKindD :: DataTypeKind
  , typeArgD  :: [TypeD]
  , typeOriginal:: (Name,DataType)
  } deriving (Show)

data TypeD = TypeD
  { tName      :: Name
  , tNamespace :: [Name]
  , tCons      :: [ConsD]
  , tMeta      :: Maybe Meta
  } deriving (Show)

data ConsD = ConsD
  { cName   :: Name
  , cFields :: [FieldDefinition]
  } deriving (Show)