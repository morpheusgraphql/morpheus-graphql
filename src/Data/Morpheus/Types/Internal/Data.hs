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
  , DataLeaf(..)
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
  , OperationKind(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , Name
  , Description
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

data OperationKind
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
  | KindObject (Maybe OperationKind)
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

data DataLeaf
  = BaseScalar DataScalar
  | CustomScalar DataScalar
  | LeafEnum DataEnum
  deriving (Show)

-- DATA KIND
data DataKind
  = ScalarKind DataScalar
  | EnumKind DataEnum
  | ObjectKind DataObject
  | UnionKind DataUnion
  deriving (Show)

data RawDataType
  = FinalDataType DataFullType
  | Interface DataObject
  | Implements { implementsInterfaces :: [Key]
               , unImplements         :: DataObject }
  deriving (Show)

data DataFullType
  = Leaf DataLeaf
  | InputObject DataObject
  | OutputObject DataObject
  | Union DataUnion
  | InputUnion DataUnion
  deriving (Show)

data DataTypeLib = DataTypeLib
  { leaf         :: [(Key, DataLeaf)]
  , inputObject  :: [(Key, DataObject)]
  , object       :: [(Key, DataObject)]
  , union        :: [(Key, DataUnion)]
  , inputUnion   :: [(Key, DataUnion)]
  , query        :: (Key, DataObject)
  , mutation     :: Maybe (Key, DataObject)
  , subscription :: Maybe (Key, DataObject)
  } deriving (Show)

initTypeLib :: (Key, DataObject) -> DataTypeLib
initTypeLib query =
  DataTypeLib
    { leaf = []
    , inputObject = []
    , query = query
    , object = []
    , union = []
    , inputUnion = []
    , mutation = Nothing
    , subscription = Nothing
    }

allDataTypes :: DataTypeLib -> [(Key, DataFullType)]
allDataTypes (DataTypeLib leaf' inputObject' object' union' inputUnion' query' mutation' subscription') =
  packType OutputObject query' :
  fromMaybeType mutation' ++
  fromMaybeType subscription' ++
  map (packType Leaf) leaf' ++
  map (packType InputObject) inputObject' ++
  map (packType InputUnion) inputUnion' ++ map (packType OutputObject) object' ++ map (packType Union) union'
  where
    packType f (x, y) = (x, f y)
    fromMaybeType :: Maybe (Key, DataObject) -> [(Key, DataFullType)]
    fromMaybeType (Just (key', dataType')) = [(key', OutputObject dataType')]
    fromMaybeType Nothing                  = []

lookupDataType :: Key -> DataTypeLib -> Maybe DataFullType
lookupDataType name lib = name `lookup` allDataTypes lib

kindOf :: DataFullType -> DataTypeKind
kindOf (Leaf (BaseScalar _))   = KindScalar
kindOf (Leaf (CustomScalar _)) = KindScalar
kindOf (Leaf (LeafEnum _))     = KindEnum
kindOf (InputObject _)         = KindInputObject
kindOf (OutputObject _)        = KindObject Nothing
kindOf (Union _)               = KindUnion
kindOf (InputUnion _)          = KindInputUnion

fromDataType :: (DataTyCon () -> v) -> DataFullType -> v
fromDataType f (Leaf (BaseScalar dt))   = f dt {typeData = ()}
fromDataType f (Leaf (CustomScalar dt)) = f dt {typeData = ()}
fromDataType f (Leaf (LeafEnum dt))     = f dt {typeData = ()}
fromDataType f (Union dt)               = f dt {typeData = ()}
fromDataType f (InputObject dt)         = f dt {typeData = ()}
fromDataType f (InputUnion dt)          = f dt {typeData = ()}
fromDataType f (OutputObject dt)        = f dt {typeData = ()}

isTypeDefined :: Key -> DataTypeLib -> Maybe DataFingerprint
isTypeDefined name lib = fromDataType typeFingerprint <$> lookupDataType name lib

defineType :: (Key, DataFullType) -> DataTypeLib -> DataTypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
defineType (key', InputUnion type') lib   = lib {inputUnion = (key', type') : inputUnion lib}

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
