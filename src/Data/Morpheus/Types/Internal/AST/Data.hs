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
  ( ScalarDefinition(..)
  , DataEnum
  , FieldsDefinition(..)
  , DataArgument
  , DataUnion
  , ArgumentsDefinition(..)
  , FieldDefinition(..)
  , DataTypeContent(..)
  , DataType(..)
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
  , createEnumValue
  , insertType
  , lookupDeprecated
  , lookupDeprecatedReason
  , checkForUnknownKeys
  , checkNameCollision
  , hasArguments
  , lookupWith
  , selectTypeObject
  )
where

import           Data.HashMap.Lazy              ( HashMap
                                                , empty
                                                , insert
                                                , union
                                                , elems
                                                )
import qualified Data.HashMap.Lazy             as HM
import           Data.Semigroup                 ( Semigroup(..), (<>) )
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
                                                , DataTypeKind(..)
                                                , DataFingerprint(..)
                                                , isNullable
                                                , sysFields
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


class Listable c a where 
  fromList     :: [a] ->  c
  toList   ::  c  -> [a]

class Selectable c a where 
  selectOr :: d -> (a -> d) -> Name -> c -> d
  selectBy :: (Failure e m, Monad m) => e -> Name -> c -> m a
  selectBy err = selectOr (failure err) pure

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
  , query        :: DataType
  , mutation     :: Maybe DataType
  , subscription :: Maybe DataType
  } deriving (Show)

type TypeLib = HashMap Key DataType

instance Selectable Schema DataType where 
  selectOr fb f name lib = maybe fb f (lookupDataType name lib)

initTypeLib :: DataType -> Schema
initTypeLib query = Schema { types        = empty
                             , query        = query
                             , mutation     = Nothing
                             , subscription = Nothing
                            }


allDataTypes :: Schema -> [DataType]
allDataTypes  = elems . typeRegister

typeRegister :: Schema -> TypeLib
typeRegister Schema { types, query, mutation, subscription } =
  types `union` HM.fromList
    (concatMap fromOperation [Just query, mutation, subscription])

createDataTypeLib :: [DataType] -> Validation Schema
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
  = DataScalar      { dataScalar        :: ScalarDefinition   }
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
createScalarType typeName = createType typeName $ DataScalar (ScalarDefinition pure)

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
    >>= mapM (flip (selectTypeObject gqlError) lib)
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

defineType :: DataType -> Schema -> Schema
defineType dt@DataType { typeName, typeContent = DataInputUnion enumKeys, typeFingerprint } lib
  = lib { types = insert name unionTags (insert typeName dt (types lib)) }
 where
  name      = typeName <> "Tags"
  unionTags = DataType
    { typeName        = name
    , typeFingerprint
    , typeMeta        = Nothing
    , typeContent     = DataEnum $ map (createEnumValue . fst) enumKeys
    }
defineType datatype lib =
  lib { types = insert (typeName datatype) datatype (types lib) }

insertType :: DataType -> TypeUpdater
insertType  datatype@DataType { typeName } lib = case isTypeDefined typeName lib of
  Nothing -> resolveUpdates (defineType datatype lib) []
  Just fingerprint | fingerprint == typeFingerprint datatype -> return lib
                   |
      -- throw error if 2 different types has same name
                     otherwise -> failure $ nameCollisionError typeName

lookupWith :: Eq k => (a -> k) -> k -> [a] -> Maybe a  
lookupWith f key = find ((== key) . f)  

-- lookups and removes DataType from hashmap 
popByKey :: Name -> [DataType] -> (Maybe DataType,[DataType])
popByKey name lib = case lookupWith typeName name lib of
    Just dt@DataType { typeContent = DataObject {} } ->
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

-- TODO: find better solution with OrderedMap to stote Fields
newtype FieldsDefinition = FieldsDefinition 
-- { unFieldsDefinition :: HashMap Name FieldDefinition } deriving (Show)
 { unFieldsDefinition :: [FieldDefinition] } deriving (Show, Lift)

-- instance Lift FieldsDefinition where 
--   lift (FieldsDefinition  hm) = [| FieldsDefinition $ HM.fromList ls |]
--     where ls = HM.toList hm

instance Semigroup FieldsDefinition where 
  FieldsDefinition x <> FieldsDefinition y = FieldsDefinition (x <> y)

instance Listable FieldsDefinition FieldDefinition where
  fromList = FieldsDefinition -- . HM.fromList 
  toList = {- HM.toList . -} unFieldsDefinition

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
fieldVisibility FieldDefinition { fieldName } = not (fieldName `elem` sysFields)

isFieldNullable :: FieldDefinition -> Bool
isFieldNullable = isNullable . fieldType

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

lookupField :: Failure error m => Key -> [(Key, field)] -> error -> m field
lookupField key fields gqlError = case lookup key fields of
  Nothing    -> failure gqlError
  Just field -> pure field

selectTypeObject :: (Monad m, Failure err m) => err -> Name -> Schema -> m (Name, FieldsDefinition )
selectTypeObject  err name lib = selectBy err name lib >>= coerceDataObject err

lookupSelectionField
  :: Failure GQLErrors Validation
  => Position
  -> Name
  -> Name
  -> FieldsDefinition
  -> Validation FieldDefinition
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
  = selectTypeObject gqlError typeConName lib
  where gqlError = hasNoSubfields key typeConName position

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

data ArgumentsDefinition 
  = ArgumentsDefinition  
    { argumentsTypename ::  Maybe Name
    , arguments         :: [DataArgument]
    }
  | NoArguments
  deriving (Show, Lift)

type DataArgument = FieldDefinition 

createArgument :: Key -> ([TypeWrapper], Key) -> FieldDefinition
createArgument = createField NoArguments

hasArguments :: ArgumentsDefinition -> Bool
hasArguments NoArguments = False
hasArguments _ = True

instance Selectable ArgumentsDefinition DataArgument where
  selectOr fb _ _    NoArguments                  = fb
  selectOr fb f key (ArgumentsDefinition _ args)  = selectOr fb f key args 

instance Listable ArgumentsDefinition DataArgument where
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
  , typeOriginal:: DataType
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
checkNameCollision enhancedKeys errorGenerator =
  case enhancedKeys \\ removeDuplicates enhancedKeys of
    []         -> pure enhancedKeys
    duplicates -> failure $ errorGenerator duplicates

checkForUnknownKeys :: Failure e m => [Ref] -> [Name] -> ([Ref] -> e) -> m [Ref]
checkForUnknownKeys enhancedKeys keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys of
    []           -> pure enhancedKeys
    unknownKeys -> failure $ errorGenerator' unknownKeys