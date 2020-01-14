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
  ( ScalarDefinition(..)
  , DataEnum
  , FieldsDefinition(..)
  , ArgumentDefinition
  , DataUnion
  , ArgumentsDefinition(..)
  , FieldDefinition(..)
  , TypeContent(..)
  , TypeDefinition(..)
  , Schema(..)
  , DataEnumValue(..)
  , TypeLib
  , Meta(..)
  , Directive(..)
  , TypeUpdater
  , TypeD(..)
  , ConsD(..)
  , ClientQuery(..)
  , GQLTypeD(..)
  , ClientType(..)
  , DataInputUnion
  , Selectable(..)
  , Listable(..)
  , isTypeDefined
  , initTypeLib
  , defineType
  , isFieldNullable
  , allDataTypes
  , lookupDataType
  , kindOf
  , toNullableField
  , toListField
  , isEntNode
  , lookupInputType
  , coerceDataObject
  , lookupDataUnion
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
  , createEnumValue
  , insertType
  , lookupDeprecated
  , lookupDeprecatedReason
  , checkForUnknownKeys
  , checkNameCollision
  , hasArguments
  , lookupWith
  , selectTypeObject
  , toHSFieldDefinition
  , Empty(..)
  )
where

import           Data.HashMap.Lazy              ( HashMap
                                                , union
                                                , elems
                                                )
import qualified Data.HashMap.Lazy             as HM
import           Data.Semigroup                 ( Semigroup(..), (<>) )
import           Language.Haskell.TH.Syntax     ( Lift(..) )
import           Instances.TH.Lift              ( )
import           Data.List                      ( find)

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
                                                , uniqueElemOr
                                                , DataTypeKind(..)
                                                , DataFingerprint(..)
                                                , isNullable
                                                , sysFields
                                                , Fields(..)
                                                , toOperationType
                                                , hsTypeName
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


class Empty a where 
  empty :: a

instance Empty (HashMap k v) where
  empty = HM.empty

instance Empty (Fields a) where 
  empty = Fields [] empty

class UniqueKey a where
  uniqueKey :: a -> Name 

class Listable c a where
  fromList     :: [a] ->  c
  toList   ::  c  -> [a]

instance Semigroup (Fields a) where 
  -- TODO: throw error on dupplicate fields
  Fields names1 values1 <> Fields names2 values2 = Fields (names1 <> names2) (values1 <> values2)

instance UniqueKey a => Listable (Fields a) a where
  fromList = collect Fields { fieldNames = [] , fieldValues = empty }
    where
      collect :: UniqueKey a => Fields a -> [a] -> Fields a
      collect fields [] = fields
      collect fields (value:values)
              | key `elem` fieldNames fields = error "TODO:error"
              | otherwise = collect (insert key value fields) values
            where key = uniqueKey value
      insert key value (Fields keys values) = Fields (keys<> [key]) (HM.insert key value values) 
  toList Fields { fieldNames , fieldValues } = maybe (error "TODO:error") id $ traverse (`HM.lookup` fieldValues) fieldNames

class Selectable c a where 
  selectOr :: d -> (a -> d) -> Name -> c -> d
  selectBy :: (Failure e m, Monad m) => e -> Name -> c -> m a
  selectBy err = selectOr (failure err) pure

instance Selectable (Fields a) a where 
  selectOr fb f key Fields { fieldValues } = maybe fb f (HM.lookup key fieldValues)

instance Selectable [(Name, a)] a where 
  selectOr fb f key lib = maybe fb f (lookup key lib)

instance Selectable (HashMap Name a) a where 
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

type DataEnum = [DataEnumValue]
type DataUnion = [Key]
type DataInputUnion = [(Key, Bool)]

-- scalar
------------------------------------------------------------------
newtype ScalarDefinition = ScalarDefinition
  { validateValue :: ValidValue -> Either Key ValidValue
  }

instance Show ScalarDefinition where
  show _ = "ScalarDefinition"

-- directive
------------------------------------------------------------------
data Directive = Directive {
  directiveName :: Name,
  directiveArgs :: [(Name, ValidValue)]
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

-- META
data Meta = Meta {
    metaDescription:: Maybe Description,
    metaDirectives  :: [Directive]
} deriving (Show,Lift)


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
  { types        :: TypeLib
  , query        :: TypeDefinition
  , mutation     :: Maybe TypeDefinition
  , subscription :: Maybe TypeDefinition
  } deriving (Show)

type TypeLib = HashMap Key TypeDefinition

instance Selectable Schema TypeDefinition where 
  selectOr fb f name lib = maybe fb f (lookupDataType name lib)

initTypeLib :: TypeDefinition -> Schema
initTypeLib query = Schema { types        = empty
                             , query        = query
                             , mutation     = Nothing
                             , subscription = Nothing
                            }


allDataTypes :: Schema -> [TypeDefinition]
allDataTypes  = elems . typeRegister

typeRegister :: Schema -> TypeLib
typeRegister Schema { types, query, mutation, subscription } =
  types `union` HM.fromList
    (concatMap fromOperation [Just query, mutation, subscription])

createDataTypeLib :: [TypeDefinition] -> Validation Schema
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

data TypeDefinition = TypeDefinition
  { typeName        :: Key
  , typeFingerprint :: DataFingerprint
  , typeMeta        :: Maybe Meta
  , typeContent     :: TypeContent
  } deriving (Show)

data TypeContent
  = DataScalar      { dataScalar        :: ScalarDefinition   }
  | DataEnum        { enumMembers       :: DataEnum     }
  | DataInputObject { inputObjectFields :: FieldsDefinition   }
  | DataObject      { objectImplements  :: [Name],
                      objectFields      :: FieldsDefinition   }
  | DataUnion       { unionMembers      :: DataUnion    }
  | DataInputUnion  { inputUnionMembers :: [(Key,Bool)] }
  | DataInterface   { interfaceFields   :: FieldsDefinition    }
  deriving (Show)

createType :: Key -> TypeContent -> TypeDefinition
createType typeName typeContent = TypeDefinition
  { typeName
  , typeMeta        = Nothing
  , typeFingerprint = DataFingerprint typeName []
  , typeContent
  }

createScalarType :: Name -> TypeDefinition
createScalarType typeName = createType typeName $ DataScalar (ScalarDefinition pure)

createEnumType :: Name -> [Key] -> TypeDefinition
createEnumType typeName typeData = createType typeName (DataEnum enumValues)
  where enumValues = map createEnumValue typeData

createEnumValue :: Name -> DataEnumValue
createEnumValue enumName = DataEnumValue { enumName, enumMeta = Nothing }

createUnionType :: Key -> [Key] -> TypeDefinition
createUnionType typeName typeData = createType typeName (DataUnion typeData)

isEntNode :: TypeContent -> Bool
isEntNode DataScalar{}  = True
isEntNode DataEnum{} = True
isEntNode _ = False

isInputDataType :: TypeDefinition -> Bool
isInputDataType TypeDefinition { typeContent } = __isInput typeContent
 where
  __isInput DataScalar{}      = True
  __isInput DataEnum{}        = True
  __isInput DataInputObject{} = True
  __isInput DataInputUnion{}  = True
  __isInput _                 = False

coerceDataObject :: Failure error m => error -> TypeDefinition -> m (Name, FieldsDefinition)
coerceDataObject _ TypeDefinition { typeContent = DataObject { objectFields } , typeName } = pure (typeName, objectFields)
coerceDataObject gqlError _ = failure gqlError

coerceDataUnion :: Failure error m => error -> TypeDefinition -> m DataUnion
coerceDataUnion _ TypeDefinition { typeContent = DataUnion members } = pure members
coerceDataUnion gqlError _ = failure gqlError

kindOf :: TypeDefinition -> DataTypeKind
kindOf TypeDefinition { typeName, typeContent } = __kind typeContent
 where
  __kind DataScalar      {} = KindScalar
  __kind DataEnum        {} = KindEnum
  __kind DataInputObject {} = KindInputObject
  __kind DataObject      {} = KindObject (toOperationType typeName)
  __kind DataUnion       {} = KindUnion
  __kind DataInputUnion  {} = KindInputUnion
  -- TODO:
  -- __kind DataInterface   {} = KindInterface

fromOperation :: Maybe TypeDefinition -> [(Name, TypeDefinition)]
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
    >>= mapM (flip (selectTypeObject gqlError) lib)
  where gqlError = hasNoSubfields key typeName position

lookupDataUnion
  :: (Monad m, Failure e m) => e -> Key -> Schema -> m DataUnion
lookupDataUnion validationError name lib =
  selectBy validationError name lib >>= coerceDataUnion validationError

lookupDataType :: Key -> Schema -> Maybe TypeDefinition
lookupDataType name  = HM.lookup name . typeRegister

lookupInputType :: Failure e m => Key -> Schema -> e -> m TypeDefinition
lookupInputType name lib errors = case lookupDataType name lib of
  Just x | isInputDataType x -> pure x
  _                          -> failure errors

isTypeDefined :: Key -> Schema -> Maybe DataFingerprint
isTypeDefined name lib = typeFingerprint <$> lookupDataType name lib

defineType :: TypeDefinition -> Schema -> Schema
defineType dt@TypeDefinition { typeName, typeContent = DataInputUnion enumKeys, typeFingerprint } lib
  = lib { types = HM.insert name unionTags (HM.insert typeName dt (types lib)) }
 where
  name      = typeName <> "Tags"
  unionTags = TypeDefinition
    { typeName        = name
    , typeFingerprint
    , typeMeta        = Nothing
    , typeContent     = DataEnum $ map (createEnumValue . fst) enumKeys
    }
defineType datatype lib =
  lib { types = HM.insert (typeName datatype) datatype (types lib) }

insertType :: TypeDefinition -> TypeUpdater
insertType  datatype@TypeDefinition { typeName } lib = case isTypeDefined typeName lib of
  Nothing -> resolveUpdates (defineType datatype lib) []
  Just fingerprint | fingerprint == typeFingerprint datatype -> return lib
                   |
      -- throw error if 2 different types has same name
                     otherwise -> failure $ nameCollisionError typeName

lookupWith :: Eq k => (a -> k) -> k -> [a] -> Maybe a  
lookupWith f key = find ((== key) . f)  

-- lookups and removes TypeDefinition from hashmap 
popByKey :: Name -> [TypeDefinition] -> (Maybe TypeDefinition,[TypeDefinition])
popByKey name lib = case lookupWith typeName name lib of
    Just dt@TypeDefinition { typeContent = DataObject {} } ->
      (Just dt, filter ((/= name) . typeName) lib)
    _ -> (Nothing, lib) 

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
 { unFieldsDefinition :: Fields FieldDefinition } deriving (Show)


instance Empty FieldsDefinition where 
  empty = FieldsDefinition empty

instance Semigroup FieldsDefinition where 
  FieldsDefinition x <> FieldsDefinition y = FieldsDefinition (x <> y)

instance UniqueKey FieldDefinition where
  uniqueKey = fieldName

instance Listable FieldsDefinition FieldDefinition where
  fromList = FieldsDefinition . fromList 
  toList = toList . unFieldsDefinition

instance Selectable FieldsDefinition FieldDefinition where
  selectOr fb f name (FieldsDefinition lib) = selectOr fb f name lib

instance Selectable [FieldDefinition] FieldDefinition where
  selectOr fb f name ls = maybe fb f (lookupWith fieldName name ls)

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
-- 
data FieldDefinition = FieldDefinition
  { fieldName     :: Key
  , fieldArgs     :: ArgumentsDefinition
  , fieldType     :: TypeRef
  , fieldMeta     :: Maybe Meta
  } deriving (Show,Lift)

fieldVisibility :: FieldDefinition -> Bool
fieldVisibility FieldDefinition { fieldName } = fieldName `notElem` sysFields

isFieldNullable :: FieldDefinition -> Bool
isFieldNullable = isNullable . fieldType

createField :: ArgumentsDefinition -> Key -> ([TypeWrapper], Key) -> FieldDefinition
createField dataArguments fieldName (typeWrappers, typeConName) = FieldDefinition
  { fieldArgs = dataArguments
  , fieldName
  , fieldType     = TypeRef { typeConName, typeWrappers, typeArgs = Nothing }
  , fieldMeta     = Nothing
  }

toHSFieldDefinition :: FieldDefinition -> FieldDefinition
toHSFieldDefinition field@FieldDefinition { fieldType = tyRef@TypeRef { typeConName } } = field 
  { fieldType = tyRef { typeConName = hsTypeName typeConName } }

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

selectTypeObject :: (Monad m, Failure err m) => err -> Name -> Schema -> m (Name, FieldsDefinition )
selectTypeObject  err name lib = selectBy err name lib >>= coerceDataObject err

lookupSelectionField
  :: Failure GQLErrors Validation
  => Position
  -> Name
  -> Name
  -> FieldsDefinition
  -> Validation FieldDefinition
lookupSelectionField position fieldName typeName = selectBy gqlError fieldName 
  where gqlError = cannotQueryField fieldName typeName position

lookupFieldAsSelectionSet
  :: (Monad m, Failure GQLErrors m)
  => Position
  -> Key
  -> Schema
  -> FieldDefinition  
  -> m (Name, FieldsDefinition )
lookupFieldAsSelectionSet position key lib FieldDefinition { fieldType = TypeRef { typeConName } }
  = selectTypeObject gqlError typeConName lib
  where gqlError = hasNoSubfields key typeConName position

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

data ArgumentsDefinition 
  = ArgumentsDefinition  
    { argumentsTypename ::  Maybe Name
    , arguments         :: [ArgumentDefinition]
    }
  | NoArguments
  deriving (Show, Lift)

type ArgumentDefinition = FieldDefinition 

createArgument :: Key -> ([TypeWrapper], Key) -> FieldDefinition
createArgument = createField NoArguments

hasArguments :: ArgumentsDefinition -> Bool
hasArguments NoArguments = False
hasArguments _ = True

instance Selectable ArgumentsDefinition ArgumentDefinition where
  selectOr fb _ _    NoArguments                  = fb
  selectOr fb f key (ArgumentsDefinition _ args)  = selectOr fb f key args 

instance Listable ArgumentsDefinition ArgumentDefinition where
  toList NoArguments                  = []
  toList (ArgumentsDefinition _ args) = args
  fromList []                         = NoArguments
  fromList args                       = ArgumentsDefinition Nothing args

-- InputValueDefinition
--   Description(opt) Name: TypeDefaultValue(opt) Directives[Const](opt)

createInputUnionFields :: Key -> [Key] -> [FieldDefinition]
createInputUnionFields name members = fieldTag : map unionField members
 where
  fieldTag = FieldDefinition 
    { fieldName = "__typename"
    , fieldArgs     = NoArguments
    , fieldType     = createAlias (name <> "Tags")
    , fieldMeta     = Nothing
    }
  unionField memberName = FieldDefinition
      { fieldArgs     = NoArguments
      , fieldName     = memberName
      , fieldType     = TypeRef { typeConName    = memberName
                                  , typeWrappers = [TypeMaybe]
                                  , typeArgs     = Nothing
                                  }
      , fieldMeta     = Nothing
      }
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
  , typeOriginal:: TypeDefinition
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


-- Helpers
-------------------------------------------------------------------------
checkNameCollision :: (Failure e m, Ord a) => [a] -> ([a] -> e) -> m [a]
checkNameCollision names toError = uniqueElemOr (failure . toError) names 

checkForUnknownKeys :: Failure e m => [Ref] -> [Name] -> ([Ref] -> e) -> m [Ref]
checkForUnknownKeys enhancedKeys keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys of
    []           -> pure enhancedKeys
    unknownKeys -> failure $ errorGenerator' unknownKeys