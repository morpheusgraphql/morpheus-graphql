{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Data.Morpheus.Types.Internal.Data
  ( Key
  , DataScalar
  , DataEnum
  , DataObject
  , DataArgument
  , DataUnion
  , DataArguments
  , DataField(..)
  , DataTyCon(..)
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
  , Name
  , Description
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
  ) where

import           Data.HashMap.Lazy                       (HashMap, empty, fromList, insert, toList, union)
import qualified Data.HashMap.Lazy                       as HM (lookup)
import           Data.Semigroup                          ((<>))
import qualified Data.Text                               as T (pack, unpack)
import           GHC.Fingerprint.Type                    (Fingerprint)
import           Language.Haskell.TH.Syntax              (Lift (..))

-- MORPHEUS
import           Data.Morpheus.Error.Internal            (internalError)
import           Data.Morpheus.Error.Selection           (cannotQueryField, hasNoSubfields)
import           Data.Morpheus.Types.Internal.Base       (Key, Position)
import           Data.Morpheus.Types.Internal.TH         (apply, liftMaybeText, liftText, liftTextMap)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Internal.Value      (Value (..))

type GenError error a = error -> Either error a

type Name = Key
type Description = Key

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
  ["__Schema", "__Type", "__Directive", "__TypeKind", "__Field", "__DirectiveLocation", "__InputValue", "__EnumValue"]

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Lift)

isSubscription :: DataTypeKind -> Bool
isSubscription (KindObject (Just Subscription)) = True
isSubscription _                                = False

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
isNullable (TypeMaybe:_) = True
isNullable _             = False

isWeaker :: [TypeWrapper] -> [TypeWrapper] -> Bool
isWeaker (TypeMaybe:xs1) (TypeMaybe:xs2) = isWeaker xs1 xs2
isWeaker (TypeMaybe:_) _                 = True
isWeaker (_:xs1) (_:xs2)                 = isWeaker xs1 xs2
isWeaker _ _                             = False

toGQLWrapper :: [TypeWrapper] -> [DataTypeWrapper]
toGQLWrapper (TypeMaybe:(TypeMaybe:tw)) = toGQLWrapper (TypeMaybe : tw)
toGQLWrapper (TypeMaybe:(TypeList:tw))  = ListType : toGQLWrapper tw
toGQLWrapper (TypeList:tw)              = [NonNullType, ListType] <> toGQLWrapper tw
toGQLWrapper [TypeMaybe]                = []
toGQLWrapper []                         = [NonNullType]

toHSWrappers :: [DataTypeWrapper] -> [TypeWrapper]
toHSWrappers (NonNullType:(NonNullType:xs)) = toHSWrappers (NonNullType : xs)
toHSWrappers (NonNullType:(ListType:xs))    = TypeList : toHSWrappers xs
toHSWrappers (ListType:xs)                  = [TypeMaybe, TypeList] <> toHSWrappers xs
toHSWrappers []                             = [TypeMaybe]
toHSWrappers [NonNullType]                  = []

data DataFingerprint
  = SystemFingerprint Key
  | TypeableFingerprint [Fingerprint]
  deriving (Show, Eq, Ord)

newtype DataValidator = DataValidator
  { validateValue :: Value -> Either Key Value
  }

instance Show DataValidator where
  show _ = "DataValidator"

type DataScalar = DataTyCon DataValidator

type DataEnum = DataTyCon [Key]

type DataObject = DataTyCon [(Key, DataField)]

type DataArgument = DataField

type DataUnion = DataTyCon [Key]

type DataArguments = [(Key, DataArgument)]

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show, Lift)

data TypeAlias = TypeAlias
  { aliasTyCon    :: Key
  , aliasArgs     :: Maybe Key
  , aliasWrappers :: [TypeWrapper]
  } deriving (Show)

instance Lift TypeAlias where
  lift TypeAlias {aliasTyCon = x, aliasArgs, aliasWrappers} =
    [|TypeAlias {aliasTyCon = name, aliasArgs = T.pack <$> args, aliasWrappers}|]
    where
      name = T.unpack x
      args = T.unpack <$> aliasArgs

data ArgsType = ArgsType
  { argsTypeName :: Key
  , resKind      :: ResolverKind
  } deriving (Show)

instance Lift ArgsType where
  lift (ArgsType argT kind) = apply 'ArgsType [liftText argT, lift kind]


data Directive = Directive deriving (Lift,Show)

-- META

data Meta = Meta {
    metaDescription:: Maybe Description,
    metaDeprecated  :: Maybe Description,
    metaDirectives  :: [Directive]
} deriving (Show)

instance Lift Meta where
  lift Meta {..} =
    apply 'Meta [
        liftMaybeText metaDescription,
        liftMaybeText metaDeprecated,
        lift metaDirectives
    ]


--
-- Data FIELD
--------------------------------------------------------------------------------------------------
data DataField = DataField
  { fieldName     :: Key
  , fieldArgs     :: [(Key, DataArgument)]
  , fieldArgsType :: Maybe ArgsType
  , fieldType     :: TypeAlias
  , fieldMeta     :: Maybe Meta
  } deriving (Show)

fieldVisibility :: (Key,DataField) -> Bool
fieldVisibility ("__typename",_) = False
fieldVisibility ("__schema",_)   = False
-- TODO: all
fieldVisibility _                = True

instance Lift DataField where
  lift DataField {..} =
    apply 'DataField [
        liftText fieldName, 
        liftTextMap fieldArgs, 
        lift fieldArgsType, 
        lift fieldType, 
        lift fieldMeta
    ]


createField :: DataArguments -> Key -> ([TypeWrapper], Key) -> DataField
createField fieldArgs fieldName (aliasWrappers, aliasTyCon) =
  DataField
    { fieldArgs
    , fieldArgsType = Nothing
    , fieldName
    , fieldType = TypeAlias {aliasTyCon, aliasWrappers, aliasArgs = Nothing}
    , fieldMeta = Nothing
    }

createArgument :: Key -> ([TypeWrapper], Key) -> (Key, DataField)
createArgument fieldName x = (fieldName, createField [] fieldName x)


toNullableField :: DataField -> DataField
toNullableField dataField
  | isNullable (aliasWrappers $ fieldType dataField) = dataField
  | otherwise = dataField {fieldType = nullable (fieldType dataField)}
  where
    nullable alias@TypeAlias {aliasWrappers} = alias {aliasWrappers = TypeMaybe : aliasWrappers}

toListField :: DataField -> DataField
toListField dataField = dataField {fieldType = listW (fieldType dataField)}
  where
    listW alias@TypeAlias {aliasWrappers} = alias {aliasWrappers = TypeList : aliasWrappers}

lookupField :: Key -> [(Key, field)] -> GenError error field
lookupField key fields gqlError =
  case lookup key fields of
    Nothing    -> Left gqlError
    Just field -> pure field

lookupSelectionField :: Position -> Key -> DataObject -> Validation DataField
lookupSelectionField position fieldName DataTyCon {typeData ,typeName } = lookupField fieldName typeData gqlError
  where
    gqlError = cannotQueryField fieldName typeName position

--
-- TYPE CONSTRUCTOR
--------------------------------------------------------------------------------------------------
data DataTyCon a = DataTyCon
  { typeName        :: Key
  , typeFingerprint :: DataFingerprint
  , typeDescription :: Maybe Key
  , typeData        :: a
  } deriving (Show)

data RawDataType
  = FinalDataType DataType
  | Interface DataObject
  | Implements { implementsInterfaces :: [Key]
               , unImplements         :: DataObject }
  deriving (Show)

--
-- DATA TYPE
--------------------------------------------------------------------------------------------------
data DataType
  = DataScalar DataScalar
  | DataEnum DataEnum
  | DataInputObject DataObject
  | DataObject DataObject
  | DataUnion DataUnion
  | DataInputUnion DataUnion
  deriving (Show)

createType :: Key -> a -> DataTyCon a
createType typeName typeData =
  DataTyCon {typeName, typeDescription = Nothing, typeFingerprint = SystemFingerprint "", typeData}

createScalarType :: Key -> (Key, DataType)
createScalarType typeName = (typeName, DataScalar $ createType typeName (DataValidator pure))

createEnumType :: Key -> [Key] -> (Key, DataType)
createEnumType typeName typeData = (typeName, DataEnum $ createType typeName typeData)

createUnionType :: Key -> [Key] -> (Key, DataType)
createUnionType typeName typeData = (typeName, DataUnion $ createType typeName typeData)


isEntNode :: DataType -> Bool
isEntNode DataScalar {} = True
isEntNode DataEnum {}   = True
isEntNode _             = False

isInputDataType :: DataType -> Bool
isInputDataType DataScalar {}      = True
isInputDataType DataEnum {}        = True
isInputDataType DataInputObject {} = True
isInputDataType DataInputUnion {}  = True
isInputDataType _                  = False

coerceDataObject :: error -> DataType -> Either error DataObject
coerceDataObject  _ (DataObject object) = pure object
coerceDataObject  gqlError _            = Left gqlError

coerceDataUnion :: error -> DataType -> Either error DataUnion
coerceDataUnion  _ (DataUnion object) = pure object
coerceDataUnion  gqlError _           = Left gqlError

kindOf :: DataType -> DataTypeKind
kindOf (DataScalar _)      = KindScalar
kindOf (DataEnum _)        = KindEnum
kindOf (DataInputObject _) = KindInputObject
kindOf (DataObject _)      = KindObject Nothing
kindOf (DataUnion _)       = KindUnion
kindOf (DataInputUnion _)  = KindInputUnion

fromDataType :: (DataTyCon () -> v) -> DataType -> v
fromDataType f (DataScalar dt)      = f dt {typeData = ()}
fromDataType f (DataEnum dt)        = f dt {typeData = ()}
fromDataType f (DataUnion dt)       = f dt {typeData = ()}
fromDataType f (DataInputObject dt) = f dt {typeData = ()}
fromDataType f (DataInputUnion dt)  = f dt {typeData = ()}
fromDataType f (DataObject dt)      = f dt {typeData = ()}

--
-- Type Register
--------------------------------------------------------------------------------------------------
data DataTypeLib = DataTypeLib
  { types        :: HashMap Key DataType
  , query        :: (Key,  DataObject)
  , mutation     :: Maybe (Key, DataObject)
  , subscription :: Maybe (Key, DataObject)
  } deriving (Show)

type TypeRegister = HashMap Key DataType

initTypeLib :: (Key, DataObject) -> DataTypeLib
initTypeLib query =
  DataTypeLib
    { types = empty
    , query = query
    , mutation = Nothing
    , subscription = Nothing
    }

allDataTypes :: DataTypeLib -> [(Key, DataType)]
allDataTypes DataTypeLib { types, query, mutation, subscription } =
  concatMap fromOperation [Just query, mutation, subscription] <> toList types

typeRegister :: DataTypeLib -> TypeRegister
typeRegister DataTypeLib { types, query, mutation, subscription } =
     types `union` fromList (concatMap fromOperation [Just query, mutation, subscription])

fromOperation :: Maybe (Key, DataObject) -> [(Key, DataType)]
fromOperation (Just (key', dataType')) = [(key', DataObject dataType')]
fromOperation Nothing                  = []

lookupDataType :: Key -> DataTypeLib -> Maybe DataType
lookupDataType name lib = name `HM.lookup` typeRegister lib

getDataType :: Key -> DataTypeLib -> GenError errors DataType
getDataType name lib gqlError  = case lookupDataType name lib of
   Just x -> Right x
   _      -> Left gqlError

lookupDataObject :: errors -> Key -> DataTypeLib -> Either errors DataObject
lookupDataObject validationError name lib  =
    getDataType name lib validationError >>=
    coerceDataObject  validationError

lookupDataUnion :: errors -> Key -> DataTypeLib -> Either errors DataUnion
lookupDataUnion validationError name lib  =
    getDataType name lib validationError >>=
    coerceDataUnion  validationError

lookupUnionTypes :: Position -> Key -> DataTypeLib -> DataField -> Validation [DataObject]
lookupUnionTypes position key lib DataField {fieldType = TypeAlias {aliasTyCon = typeName}} =
  lookupDataUnion gqlError typeName lib >>= mapM (flip (lookupDataObject gqlError) lib ) . typeData
  where
    gqlError = hasNoSubfields key typeName position

lookupFieldAsSelectionSet :: Position -> Key -> DataTypeLib -> DataField -> Validation DataObject
lookupFieldAsSelectionSet position key lib DataField {fieldType = TypeAlias {aliasTyCon}} =
  lookupDataObject gqlError aliasTyCon lib
  where
    gqlError = hasNoSubfields key aliasTyCon position

lookupInputType :: Key -> DataTypeLib -> GenError error DataType
lookupInputType name lib gqlError = case lookupDataType name lib of
    Just x | isInputDataType x -> Right x
    _                          -> Left gqlError

isTypeDefined :: Key -> DataTypeLib -> Maybe DataFingerprint
isTypeDefined name lib = fromDataType typeFingerprint <$> lookupDataType name lib

defineType :: (Key, DataType) -> DataTypeLib -> DataTypeLib
defineType (key, datatype@(DataInputUnion DataTyCon{ typeName ,typeData , typeFingerprint })) lib = lib {
   types = insert name unionTags (insert key datatype (types lib))
}
  where
     name = typeName <> "Tags"
     unionTags = DataEnum DataTyCon {
          typeName = name
        , typeFingerprint
        , typeDescription = Nothing
        , typeData
  }
defineType (key, datatype) lib = lib {
    types =  insert key datatype (types lib)
}

lookupType :: error -> [(Key, a)] -> Key -> Either error a
lookupType error' lib' typeName' =
  case lookup typeName' lib' of
    Nothing -> Left error'
    Just x  -> pure x


createDataTypeLib :: [(Key, DataType)] -> Validation DataTypeLib
createDataTypeLib types =
  case takeByKey "Query" types of
    (Just query, lib1) ->
      case takeByKey "Mutation" lib1 of
        (mutation, lib2) ->
          case takeByKey "Subscription" lib2 of
            (subscription, lib3) -> pure ((foldr defineType (initTypeLib query) lib3) {mutation, subscription})
    _ -> internalError "Query Not Defined"
  ----------------------------------------------------------------------------
  where
    takeByKey key lib =
      case lookup key lib of
        Just (DataObject value) -> (Just (key, value), filter ((/= key) . fst) lib)
        _                       -> (Nothing, lib)


createInputUnionFields :: Key -> [Key] -> [(Key,DataField)]
createInputUnionFields name members = fieldTag : map unionField members
    where
        fieldTag = ("tag", DataField
          { fieldName = "tag"
          , fieldArgs = []
          , fieldArgsType = Nothing
          , fieldType = createAlias (name <> "Tags")
          , fieldMeta = Nothing
          })
        unionField memberName = (
            memberName ,
            DataField {
              fieldArgs = []
              , fieldArgsType = Nothing
              , fieldName = memberName
              , fieldType = TypeAlias {
                    aliasTyCon = memberName,
                    aliasWrappers = [TypeMaybe],
                    aliasArgs = Nothing
                }
              , fieldMeta = Nothing
            }
          )

createAlias :: Key -> TypeAlias
createAlias aliasTyCon = TypeAlias {aliasTyCon, aliasWrappers = [], aliasArgs = Nothing}

