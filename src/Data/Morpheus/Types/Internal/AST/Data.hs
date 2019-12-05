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

module Data.Morpheus.Types.Internal.AST.Data
  ( DataScalar
  , DataEnum
  , DataObject
  , DataArgument
  , DataUnion
  , DataArguments
  , DataField(..)
  , DataTypeContent(..)
  , DataType(..)
  , DataTypeLib(..)
  , DataTypeWrapper(..)
  , DataValidator(..)
  , DataTypeKind(..)
  , DataFingerprint(..)
  , RawDataType(..)
  , ResolverKind(..)
  , TypeWrapper(..)
  , TypeAlias(..)
  , ArgsType(..)
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
  , getDataType
  , lookupDataObject
  , lookupDataUnion
  , lookupType
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
  )
where

import           Data.HashMap.Lazy              ( HashMap
                                                , empty
                                                , fromList
                                                , insert
                                                , toList
                                                , union
                                                )
import qualified Data.HashMap.Lazy             as HM
                                                ( lookup )
import           Data.Semigroup                 ( (<>) )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Instances.TH.Lift              ( )
import           Data.List                      ( find )

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
                                                , ScalarValue(..)
                                                )
import           Data.Morpheus.Error.Schema     ( nameCollisionError )

type QUERY = 'Query
type MUTATION = 'Mutation
type SUBSCRIPTION = 'Subscription

isDefaultTypeName :: Key -> Bool
isDefaultTypeName x = isSchemaTypeName x || isPrimitiveTypeName x

isSchemaTypeName :: Key -> Bool
isSchemaTypeName = (`elem` sysTypes)

isPrimitiveTypeName :: Key -> Bool
isPrimitiveTypeName = (`elem` ["String", "Float", "Int", "Boolean", "ID"])

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

data ResolverKind
  = PlainResolver
  | TypeVarResolver
  | ExternalResolver
  deriving (Show, Eq, Lift)

data TypeWrapper
  = TypeList
  | TypeMaybe
  deriving (Show, Lift)

isFieldNullable :: DataField -> Bool
isFieldNullable = isNullable . aliasWrappers . fieldType

isNullable :: [TypeWrapper] -> Bool
isNullable (TypeMaybe : _) = True
isNullable _               = False

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
  { validateValue :: Value -> Either Key Value
  }

instance Show DataValidator where
  show _ = "DataValidator"

type DataScalar = DataValidator
type DataEnum = [DataEnumValue]
type DataObject = [(Key, DataField)]
type DataArgument = DataField
type DataUnion = [Key]
type DataInputUnion = [(Key, Bool)]
type DataArguments = [(Key, DataArgument)]

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show, Lift)

data TypeAlias = TypeAlias
  { aliasTyCon    :: Key
  , aliasArgs     :: Maybe Key
  , aliasWrappers :: [TypeWrapper]
  } deriving (Show,Lift)

data ArgsType = ArgsType
  { argsTypeName :: Key
  , resKind      :: ResolverKind
  } deriving (Show,Lift)

data Directive = Directive {
  directiveName :: Name,
  directiveArgs :: [(Name,Value)]
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
  maybeString (Scalar (String x)) = x
  maybeString _                   = "can't read deprecated Reason Value"
  isReason ("reason", _) = True
  isReason _             = False

-- ENUM VALUE
data DataEnumValue = DataEnumValue{
    enumName :: Name,
    enumMeta :: Maybe Meta
} deriving (Show, Lift)

--------------------------------------------------------------------------------------------------
data DataField = DataField
  { fieldName     :: Key
  , fieldArgs     :: [(Key, DataArgument)]
  , fieldArgsType :: Maybe ArgsType
  , fieldType     :: TypeAlias
  , fieldMeta     :: Maybe Meta
  } deriving (Show,Lift)

fieldVisibility :: (Key, DataField) -> Bool
fieldVisibility ("__typename", _) = False
fieldVisibility ("__schema"  , _) = False
fieldVisibility ("__type"    , _) = False
fieldVisibility _                 = True

createField :: DataArguments -> Key -> ([TypeWrapper], Key) -> DataField
createField fieldArgs fieldName (aliasWrappers, aliasTyCon) = DataField
  { fieldArgs
  , fieldArgsType = Nothing
  , fieldName
  , fieldType     = TypeAlias { aliasTyCon, aliasWrappers, aliasArgs = Nothing }
  , fieldMeta     = Nothing
  }

createArgument :: Key -> ([TypeWrapper], Key) -> (Key, DataField)
createArgument fieldName x = (fieldName, createField [] fieldName x)


toNullableField :: DataField -> DataField
toNullableField dataField
  | isNullable (aliasWrappers $ fieldType dataField) = dataField
  | otherwise = dataField { fieldType = nullable (fieldType dataField) }
 where
  nullable alias@TypeAlias { aliasWrappers } =
    alias { aliasWrappers = TypeMaybe : aliasWrappers }

toListField :: DataField -> DataField
toListField dataField = dataField { fieldType = listW (fieldType dataField) }
 where
  listW alias@TypeAlias { aliasWrappers } =
    alias { aliasWrappers = TypeList : aliasWrappers }

lookupField :: Failure error m => Key -> [(Key, field)] -> error -> m field
lookupField key fields gqlError = case lookup key fields of
  Nothing    -> failure gqlError
  Just field -> pure field

lookupSelectionField
  :: Failure GQLErrors Validation
  => Position
  -> Name
  -> Name
  -> DataObject
  -> Validation DataField
lookupSelectionField position fieldName typeName fields = lookupField
  fieldName
  fields
  gqlError
  where gqlError = cannotQueryField fieldName typeName position

--
-- TYPE CONSTRUCTOR
--------------------------------------------------------------------------------------------------

data RawDataType
  = FinalDataType DataType
  | Interface {
      interfaceName    :: Key
      , interfaceMeta    :: Maybe Meta
      , interfaceContent :: DataObject
    }
  | Implements {
        implementsName    :: Key
      , implementsInterfaces :: [Key]
      , implementsMeta    :: Maybe Meta
      , implementsContent         :: DataObject
    }
  deriving (Show)

--
-- DATA TYPE
--------------------------------------------------------------------------------------------------
data DataTypeContent
  = DataScalar DataScalar
  | DataEnum DataEnum
  | DataInputObject DataObject
  | DataObject DataObject
  | DataUnion DataUnion
  | DataInputUnion [(Key,Bool)]
  deriving (Show)

data DataType = DataType
  { typeName        :: Key
  , typeFingerprint :: DataFingerprint
  , typeMeta :: Maybe Meta
  , typeContent       :: DataTypeContent
  } deriving (Show)

createType :: Key -> DataTypeContent -> DataType
createType typeName typeContent = DataType
  { typeName
  , typeMeta        = Nothing
  , typeFingerprint = DataFingerprint typeName []
  , typeContent
  }

createScalarType :: Key -> (Key, DataType)
createScalarType typeName =
  (typeName, createType typeName $ DataScalar (DataValidator pure))

createEnumType :: Key -> [Key] -> (Key, DataType)
createEnumType typeName typeData =
  (typeName, createType typeName $ DataEnum enumValues)
  where enumValues = map createEnumValue typeData

createEnumValue :: Key -> DataEnumValue
createEnumValue enumName = DataEnumValue { enumName, enumMeta = Nothing }

createUnionType :: Key -> [Key] -> (Key, DataType)
createUnionType typeName typeData =
  (typeName, createType typeName $ DataUnion typeData)


isEntNode :: DataType -> Bool
isEntNode DataType { typeContent = DataScalar{} } = True
isEntNode DataType { typeContent = DataEnum{} } = True
isEntNode _ = False

isInputDataType :: DataType -> Bool
isInputDataType DataType { typeContent } = isInput typeContent
 where
  isInput DataScalar{}      = True
  isInput DataEnum{}        = True
  isInput DataInputObject{} = True
  isInput DataInputUnion{}  = True
  isInput _                 = False

coerceDataObject :: Failure error m => error -> DataType -> m DataObject
coerceDataObject _ DataType { typeContent = DataObject object } = pure object
coerceDataObject gqlError _ = failure gqlError

coerceDataUnion :: Failure error m => error -> DataType -> m DataUnion
coerceDataUnion _ DataType { typeContent = DataUnion union } = pure union
coerceDataUnion gqlError _ = failure gqlError

kindOf :: DataType -> DataTypeKind
kindOf DataType { typeContent } = __kind typeContent
 where
  __kind (DataScalar      _) = KindScalar
  __kind (DataEnum        _) = KindEnum
  __kind (DataInputObject _) = KindInputObject
  __kind (DataObject      _) = KindObject Nothing
  __kind (DataUnion       _) = KindUnion
  __kind (DataInputUnion  _) = KindInputUnion


--
-- Type Register
--------------------------------------------------------------------------------------------------
data DataTypeLib = DataTypeLib
  { types        :: HashMap Key DataType
  , query        :: (Name,DataType)
  , mutation     :: Maybe (Name, DataType)
  , subscription :: Maybe (Name, DataType)
  } deriving (Show)

type TypeRegister = HashMap Key DataType

initTypeLib :: (Key, DataType) -> DataTypeLib
initTypeLib query = DataTypeLib { types        = empty
                                , query        = query
                                , mutation     = Nothing
                                , subscription = Nothing
                                }

allDataTypes :: DataTypeLib -> [(Key, DataType)]
allDataTypes DataTypeLib { types, query, mutation, subscription } =
  concatMap fromOperation [Just query, mutation, subscription] <> toList types

typeRegister :: DataTypeLib -> TypeRegister
typeRegister DataTypeLib { types, query, mutation, subscription } =
  types `union` fromList
    (concatMap fromOperation [Just query, mutation, subscription])

fromOperation :: Maybe (Key, DataType) -> [(Key, DataType)]
fromOperation (Just (key, datatype)) = [(key, datatype)]
fromOperation Nothing = []

lookupDataType :: Key -> DataTypeLib -> Maybe DataType
lookupDataType name lib = name `HM.lookup` typeRegister lib

getDataType :: Failure error m => Key -> DataTypeLib -> error -> m DataType
getDataType name lib gqlError = case lookupDataType name lib of
  Just x -> pure x
  _      -> failure gqlError

lookupDataObject
  :: (Monad m, Failure e m) => e -> Key -> DataTypeLib -> m DataObject
lookupDataObject validationError name lib =
  getDataType name lib validationError >>= coerceDataObject validationError

lookupDataUnion
  :: (Monad m, Failure e m) => e -> Key -> DataTypeLib -> m DataUnion
lookupDataUnion validationError name lib =
  getDataType name lib validationError >>= coerceDataUnion validationError

lookupUnionTypes
  :: (Monad m, Failure GQLErrors m)
  => Position
  -> Key
  -> DataTypeLib
  -> DataField
  -> m [DataObject]
lookupUnionTypes position key lib DataField { fieldType = TypeAlias { aliasTyCon = typeName } }
  = lookupDataUnion gqlError typeName lib
    >>= mapM (flip (lookupDataObject gqlError) lib)
  where gqlError = hasNoSubfields key typeName position

lookupFieldAsSelectionSet
  :: (Monad m, Failure GQLErrors m)
  => Position
  -> Key
  -> DataTypeLib
  -> DataField
  -> m DataObject
lookupFieldAsSelectionSet position key lib DataField { fieldType = TypeAlias { aliasTyCon } }
  = lookupDataObject gqlError aliasTyCon lib
  where gqlError = hasNoSubfields key aliasTyCon position

lookupInputType :: Failure e m => Key -> DataTypeLib -> e -> m DataType
lookupInputType name lib errors = case lookupDataType name lib of
  Just x | isInputDataType x -> pure x
  _                          -> failure errors

isTypeDefined :: Key -> DataTypeLib -> Maybe DataFingerprint
isTypeDefined name lib = typeFingerprint <$> lookupDataType name lib

defineType :: (Key, DataType) -> DataTypeLib -> DataTypeLib
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

lookupType :: Failure e m => e -> [(Key, a)] -> Key -> m a
lookupType err lib typeName = case lookup typeName lib of
  Nothing -> failure err
  Just x  -> pure x

createDataTypeLib :: [(Key, DataType)] -> Validation DataTypeLib
createDataTypeLib types = case takeByKey "Query" types of
  (Just query, lib1) -> case takeByKey "Mutation" lib1 of
    (mutation, lib2) -> case takeByKey "Subscription" lib2 of
      (subscription, lib3) ->
        pure
          ((foldr defineType (initTypeLib query) lib3) { mutation
                                                       , subscription
                                                       }
          )
  _ -> internalError "Query Not Defined"
 where
  takeByKey key lib = case lookup key lib of
    Just dt@DataType { typeContent = DataObject {} } ->
      (Just (key, dt), filter ((/= key) . fst) lib)
    _ -> (Nothing, lib)


createInputUnionFields :: Key -> [Key] -> [(Key, DataField)]
createInputUnionFields name members = fieldTag : map unionField members
 where
  fieldTag =
    ( "__typename"
    , DataField { fieldName     = "__typename"
                , fieldArgs     = []
                , fieldArgsType = Nothing
                , fieldType     = createAlias (name <> "Tags")
                , fieldMeta     = Nothing
                }
    )
  unionField memberName =
    ( memberName
    , DataField
      { fieldArgs     = []
      , fieldArgsType = Nothing
      , fieldName     = memberName
      , fieldType     = TypeAlias { aliasTyCon    = memberName
                                  , aliasWrappers = [TypeMaybe]
                                  , aliasArgs     = Nothing
                                  }
      , fieldMeta     = Nothing
      }
    )

createAlias :: Key -> TypeAlias
createAlias aliasTyCon =
  TypeAlias { aliasTyCon, aliasWrappers = [], aliasArgs = Nothing }


type TypeUpdater = LibUpdater DataTypeLib

insertType :: (Key, DataType) -> TypeUpdater
insertType nextType@(name, datatype) lib = case isTypeDefined name lib of
  Nothing -> resolveUpdates (defineType nextType lib) []
  Just fingerprint | fingerprint == typeFingerprint datatype -> return lib
                   |
      -- throw error if 2 different types has same name
                     otherwise -> failure $ nameCollisionError name


-- TEMPLATE HASKELL DATA TYPES

-- CLIENT                                                
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
  { tName      :: String
  , tNamespace :: [String]
  , tCons      :: [ConsD]
  , tMeta      :: Maybe Meta
  } deriving (Show)

data ConsD = ConsD
  { cName   :: String
  , cFields :: [DataField]
  } deriving (Show)
