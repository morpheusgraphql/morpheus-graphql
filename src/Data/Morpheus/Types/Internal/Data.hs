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
  , DataKind(..)
  , DataFullType(..)
  , DataTypeLib(..)
  , DataTypeWrapper(..)
  , DataValidator(..)
  , DataTypeKind(..)
  , DataFingerprint(..)
  , RawDataType(..)
  , ResolverKind(..)
  , WrapperD(..)
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
  , toDataKind
  , OperationType(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , Name
  , Description
  , isEntNode
  ) where

import           Data.Semigroup                     ((<>))
import qualified Data.Text                          as T (pack, unpack)
import           GHC.Fingerprint.Type               (Fingerprint)
import           Language.Haskell.TH.Syntax         (Lift (..))

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Base  (Key)
import           Data.Morpheus.Types.Internal.TH    (apply, liftText, liftTextMap)
import           Data.Morpheus.Types.Internal.Value (Value (..))

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

data WrapperD
  = ListD
  | MaybeD
  deriving (Show, Lift)

isFieldNullable :: DataField -> Bool
isFieldNullable = isNullable . aliasWrappers . fieldType

isNullable :: [WrapperD] -> Bool
isNullable (MaybeD:_) = True
isNullable _          = False

isWeaker :: [WrapperD] -> [WrapperD] -> Bool
isWeaker (MaybeD:xs1) (MaybeD:xs2) = isWeaker xs1 xs2
isWeaker (MaybeD:_) _              = True
isWeaker (_:xs1) (_:xs2)           = isWeaker xs1 xs2
isWeaker _ _                       = False

toGQLWrapper :: [WrapperD] -> [DataTypeWrapper]
toGQLWrapper (MaybeD:(MaybeD:tw)) = toGQLWrapper (MaybeD : tw)
toGQLWrapper (MaybeD:(ListD:tw))  = ListType : toGQLWrapper tw
toGQLWrapper (ListD:tw)           = [NonNullType, ListType] <> toGQLWrapper tw
toGQLWrapper [MaybeD]             = []
toGQLWrapper []                   = [NonNullType]

toHSWrappers :: [DataTypeWrapper] -> [WrapperD]
toHSWrappers (NonNullType:(NonNullType:xs)) = toHSWrappers (NonNullType : xs)
toHSWrappers (NonNullType:(ListType:xs))    = ListD : toHSWrappers xs
toHSWrappers (ListType:xs)                  = [MaybeD, ListD] <> toHSWrappers xs
toHSWrappers []                             = [MaybeD]
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

type DataUnion = DataTyCon [DataField]

type DataArguments = [(Key, DataArgument)]

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show, Lift)

data TypeAlias = TypeAlias
  { aliasTyCon    :: Key
  , aliasArgs     :: Maybe Key
  , aliasWrappers :: [WrapperD]
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

data DataField = DataField
  { fieldName     :: Key
  , fieldArgs     :: [(Key, DataArgument)]
  , fieldArgsType :: Maybe ArgsType
  , fieldType     :: TypeAlias
  , fieldHidden   :: Bool
  } deriving (Show)

instance Lift DataField where
  lift (DataField name args argsT ft hid) =
    apply 'DataField [liftText name, liftTextMap args, lift argsT, lift ft, lift hid]

data DataTyCon a = DataTyCon
  { typeName        :: Key
  , typeFingerprint :: DataFingerprint
  , typeDescription :: Maybe Key
  , typeData        :: a
  } deriving (Show)



-- DATA KIND
data DataKind
  = ScalarKind DataScalar
  | EnumKind DataEnum
  | ObjectKind DataObject
  | UnionKind DataUnion
  deriving (Show)

toDataKind :: DataFullType -> DataKind
toDataKind (DataScalar x)      = ScalarKind x
toDataKind (DataEnum x)        = EnumKind x
toDataKind (DataInputObject x) = ObjectKind x
toDataKind (DataObject x)      = ObjectKind x
toDataKind (DataUnion x)       = UnionKind x
toDataKind (DataInputUnion x)  = UnionKind x

data RawDataType
  = FinalDataType DataFullType
  | Interface DataObject
  | Implements { implementsInterfaces :: [Key]
               , unImplements         :: DataObject }
  deriving (Show)

isEntNode :: DataFullType -> Bool
isEntNode DataScalar {} = True
isEntNode DataEnum {}   = True
isEntNode _             = False

data DataFullType
  = DataScalar DataScalar
  | DataEnum DataEnum
  | DataInputObject DataObject
  | DataObject DataObject
  | DataUnion DataUnion
  | DataInputUnion DataUnion
  deriving (Show)

data DataTypeLib = DataTypeLib
  { scalar       :: [(Key, DataScalar)]
  , enum         :: [(Key, DataEnum)]
  , inputObject  :: [(Key, DataObject)]
  , object       :: [(Key, DataObject)]
  , union        :: [(Key, DataUnion)]
  , inputUnion   :: [(Key, DataUnion)]
  , query        :: (Key,  DataObject)
  , mutation     :: Maybe (Key, DataObject)
  , subscription :: Maybe (Key, DataObject)
  } deriving (Show)

initTypeLib :: (Key, DataObject) -> DataTypeLib
initTypeLib query =
  DataTypeLib
    { scalar = []
    , enum = []
    , inputObject = []
    , query = query
    , object = []
    , union = []
    , inputUnion = []
    , mutation = Nothing
    , subscription = Nothing
    }

allDataTypes :: DataTypeLib -> [(Key, DataFullType)]
allDataTypes DataTypeLib { scalar, enum , inputObject, object, union, inputUnion, query, mutation, subscription } =
  packType DataObject query :
  fromMaybeType mutation ++
  fromMaybeType subscription ++
  map (packType DataScalar) scalar ++
  map (packType DataEnum) enum ++
  map (packType DataInputObject) inputObject ++
  map (packType DataInputUnion) inputUnion ++ map (packType DataObject) object ++ map (packType DataUnion) union
  where
    packType f (x, y) = (x, f y)
    fromMaybeType :: Maybe (Key, DataObject) -> [(Key, DataFullType)]
    fromMaybeType (Just (key', dataType')) = [(key', DataObject dataType')]
    fromMaybeType Nothing                  = []

lookupDataType :: Key -> DataTypeLib -> Maybe DataFullType
lookupDataType name lib = name `lookup` allDataTypes lib

kindOf :: DataFullType -> DataTypeKind
kindOf (DataScalar _)      = KindScalar
kindOf (DataEnum _)        = KindEnum
kindOf (DataInputObject _) = KindInputObject
kindOf (DataObject _)      = KindObject Nothing
kindOf (DataUnion _)       = KindUnion
kindOf (DataInputUnion _)  = KindInputUnion

fromDataType :: (DataTyCon () -> v) -> DataFullType -> v
fromDataType f (DataScalar dt)      = f dt {typeData = ()}
fromDataType f (DataEnum dt)        = f dt {typeData = ()}
fromDataType f (DataUnion dt)       = f dt {typeData = ()}
fromDataType f (DataInputObject dt) = f dt {typeData = ()}
fromDataType f (DataInputUnion dt)  = f dt {typeData = ()}
fromDataType f (DataObject dt)      = f dt {typeData = ()}

isTypeDefined :: Key -> DataTypeLib -> Maybe DataFingerprint
isTypeDefined name lib = fromDataType typeFingerprint <$> lookupDataType name lib

defineType :: (Key, DataFullType) -> DataTypeLib -> DataTypeLib
defineType (key', DataScalar type') lib      = lib {scalar = (key', type') : scalar lib}
defineType (key', DataEnum type') lib        = lib {enum = (key', type') : enum lib}
defineType (key', DataInputObject type') lib = lib {inputObject = (key', type') : inputObject lib}
defineType (key', DataObject type') lib      = lib {object = (key', type') : object lib}
defineType (key', DataUnion type') lib       = lib {union = (key', type') : union lib}
defineType (key', DataInputUnion type') lib  = lib {inputUnion = (key', type') : inputUnion lib}

toNullableField :: DataField -> DataField
toNullableField dataField
  | isNullable (aliasWrappers $ fieldType dataField) = dataField
  | otherwise = dataField {fieldType = nullable (fieldType dataField)}
  where
    nullable alias@TypeAlias {aliasWrappers} = alias {aliasWrappers = MaybeD : aliasWrappers}

toListField :: DataField -> DataField
toListField dataField = dataField {fieldType = listW (fieldType dataField)}
  where
    listW alias@TypeAlias {aliasWrappers} = alias {aliasWrappers = ListD : aliasWrappers}
