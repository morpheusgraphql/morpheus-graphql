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
{-# LANGUAGE StandaloneDeriving    #-}

module Data.Morpheus.Types.Internal.AST.Data
  ( DataScalar
  , DataEnum
  , DataObject
  , DataArgument
  , DataUnion
  , DataArguments(..)
  , DataField(..)
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
  , DataLookup(..)
  , TypeCategory
  , INPUT
  , OUTPUT
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

isFieldNullable :: DataField cat -> Bool
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
type DataObject cat = [(Key, DataField cat)]
type DataArgument = DataField INPUT
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


data TypeCategory = InputType | OutputType

type INPUT = InputType
type OUTPUT = OutputType

data DataArguments (cat :: TypeCategory) where 
  DataArguments :: 
    { argumentsTypename ::  Maybe Name
    , arguments         :: [(Key, DataArgument)] 
    }  -> DataArguments OUTPUT
  NoArguments :: DataArguments INPUT

deriving instance Lift (DataArguments cat)
deriving instance Show (DataArguments cat)

--------------------------------------------------------------------------------------------------
data DataField (cat :: TypeCategory ) = DataField
  { fieldName     :: Key
  , fieldArgs     :: DataArguments cat
  , fieldType     :: TypeRef
  , fieldMeta     :: Maybe Meta
  } deriving (Show,Lift)

fieldVisibility :: (Key, DataField cat) -> Bool
fieldVisibility ("__typename", _) = False
fieldVisibility ("__schema"  , _) = False
fieldVisibility ("__type"    , _) = False
fieldVisibility _                 = True

createField :: DataArguments cat -> Key -> ([TypeWrapper], Key) -> DataField cat
createField dataArguments fieldName (typeWrappers, typeConName) = DataField
  { fieldArgs = dataArguments
  , fieldName
  , fieldType     = TypeRef { typeConName, typeWrappers, typeArgs = Nothing }
  , fieldMeta     = Nothing
  }

createArgument :: Key -> ([TypeWrapper], Key) -> (Key, DataArgument)
createArgument fieldName x = (fieldName, createField NoArguments fieldName x)


toNullableField :: DataField cat -> DataField cat
toNullableField dataField
  | isNullable (fieldType dataField) = dataField
  | otherwise = dataField { fieldType = nullable (fieldType dataField) }
 where
  nullable alias@TypeRef { typeWrappers } =
    alias { typeWrappers = TypeMaybe : typeWrappers }

toListField :: DataField cat -> DataField cat
toListField dataField = dataField { fieldType = listW (fieldType dataField) }
 where
  listW alias@TypeRef { typeWrappers } =
    alias { typeWrappers = TypeList : typeWrappers }

lookupField :: Failure error m => Key -> [(Key, field)] -> error -> m field
lookupField key fields gqlError = case lookup key fields of
  Nothing    -> failure gqlError
  Just field -> pure field

lookupSelectionField
  :: Failure GQLErrors Validation
  => Position
  -> Name
  -> Name
  -> DataObject cat
  -> Validation (DataField cat)
lookupSelectionField position fieldName typeName fields = lookupField
  fieldName
  fields
  gqlError
  where gqlError = cannotQueryField fieldName typeName position


-- DATA TYPE
--------------------------------------------------------------------------------------------------
data DataType = DataType
  { typeName        :: Key
  , typeFingerprint :: DataFingerprint
  , typeMeta        :: Maybe Meta
  , typeContent     :: DataTypeContent
  } deriving (Show)

data DataTypeContent
  = DataScalar      { dataScalar        :: DataScalar   }
  | DataEnum        { enumMembers       :: DataEnum     }
  | DataInputObject { inputObjectFields :: DataObject INPUT  }
  | DataObject      { objectImplements  :: [Name],
                      objectFields      :: DataObject OUTPUT  }
  | DataUnion       { unionMembers      :: DataUnion    }
  | DataInputUnion  { inputUnionMembers :: [(Key,Bool)] }
  | DataInterface   { interfaceFields   :: DataObject OUTPUT   }
  deriving (Show)

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

coerceDataObject :: Failure error m => error -> DataType -> m (Name, DataObject OUTPUT)
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

--
-- Type Register
--------------------------------------------------------------------------------------------------
data Schema = Schema
  { types        :: HashMap Name DataType
  , query        :: (Name,DataType)
  , mutation     :: Maybe (Name,DataType)
  , subscription :: Maybe (Name,DataType)
  } deriving (Show)

type TypeRegister = HashMap Key DataType

initTypeLib :: (Key, DataType) -> Schema
initTypeLib query = Schema { types        = empty
                             , query        = query
                             , mutation     = Nothing
                             , subscription = Nothing
                            }

allDataTypes :: Schema -> [(Key, DataType)]
allDataTypes  = toList . typeRegister

typeRegister :: Schema -> TypeRegister
typeRegister Schema { types, query, mutation, subscription } =
  types `union` fromList
    (concatMap fromOperation [Just query, mutation, subscription])

fromOperation :: Maybe (Key, DataType) -> [(Key, DataType)]
fromOperation (Just (key, datatype)) = [(key, datatype)]
fromOperation Nothing = []


class DataLookup l a where 
  lookupResult :: (Failure e m, Monad m) => e -> Name -> l -> m a 

instance DataLookup Schema DataType where 
  lookupResult err name lib = case lookupDataType name lib of
      Nothing -> failure err
      Just x  -> pure x

instance DataLookup Schema (Name, DataObject OUTPUT) where 
  lookupResult validationError name lib =
     lookupResult validationError name lib >>= coerceDataObject validationError

lookupDataUnion
  :: (Monad m, Failure e m) => e -> Key -> Schema -> m DataUnion
lookupDataUnion validationError name lib =
  lookupResult validationError name lib >>= coerceDataUnion validationError

lookupDataType :: Key -> Schema -> Maybe DataType
lookupDataType name  = HM.lookup name . typeRegister

lookupUnionTypes
  :: (Monad m, Failure GQLErrors m)
  => Position
  -> Key
  -> Schema
  -> DataField OUTPUT
  -> m [(Name, DataObject OUTPUT)]
lookupUnionTypes position key lib DataField { fieldType = TypeRef { typeConName = typeName } }
  = lookupDataUnion gqlError typeName lib
    >>= mapM (flip (lookupResult gqlError) lib)
  where gqlError = hasNoSubfields key typeName position

lookupFieldAsSelectionSet
  :: (Monad m, Failure GQLErrors m)
  => Position
  -> Key
  -> Schema
  -> DataField OUTPUT 
  -> m (Name, DataObject OUTPUT)
lookupFieldAsSelectionSet position key lib DataField { fieldType = TypeRef { typeConName } }
  = lookupResult gqlError typeConName lib
  where gqlError = hasNoSubfields key typeConName position

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


-- lookups and removes DataType from hashmap 
popByKey :: Name -> [(Key, DataType)] -> (Maybe (Name,DataType), [(Key, DataType)])
popByKey key lib = case lookup key lib of
    Just dt@DataType { typeContent = DataObject {} } ->
      (Just (key, dt), filter ((/= key) . fst) lib)
    _ -> (Nothing, lib)  


createDataTypeLib :: [(Key, DataType)] -> Validation Schema
createDataTypeLib types = case popByKey "Query" types of
  (Nothing   ,_    ) -> internalError "Query Not Defined"
  (Just query, lib1) -> do
    let (mutation, lib2) = popByKey "Mutation" lib1
    let (subscription, lib3) = popByKey "Subscription" lib2
    pure $ (foldr defineType (initTypeLib query) lib3) {mutation, subscription}


createInputUnionFields :: Key -> [Key] -> [(Key, DataField INPUT )]
createInputUnionFields name members = fieldTag : map unionField members
 where
  fieldTag =
    ( "__typename"
    , DataField { fieldName     = "__typename"
                , fieldArgs     = NoArguments
                , fieldType     = createAlias (name <> "Tags")
                , fieldMeta     = Nothing
                }
    )
  unionField memberName =
    ( memberName
    , DataField
      { fieldArgs     = NoArguments
      , fieldName     = memberName
      , fieldType     = TypeRef { typeConName    = memberName
                                  , typeWrappers = [TypeMaybe]
                                  , typeArgs     = Nothing
                                  }
      , fieldMeta     = Nothing
      }
    )

createAlias :: Key -> TypeRef
createAlias typeConName =
  TypeRef { typeConName, typeWrappers = [], typeArgs = Nothing }


type TypeUpdater = LibUpdater Schema

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
  { tName      :: Name
  , tNamespace :: [Name]
  , tCons      :: [ConsD]
  , tMeta      :: Maybe Meta
  } deriving (Show)

data ConsD = ConsD
  { cName   :: Name
  , cFields :: [DataField OUTPUT]
  } deriving (Show)