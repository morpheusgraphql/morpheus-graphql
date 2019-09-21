{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
  , isTypeDefined
  , initTypeLib
  , defineType
  , showWrappedType
  , showFullAstType
  , isFieldNullable
  , allDataTypes
  , lookupDataType
  , kindOf
  , toNullableField
  , toListField
  , unKindD
  , isObject
  , isInput
  , toHSWrappers
  , KindD(..)
  , isNullable
  , hsToGQLWrapper
  , toGQLWrapper
  , isEqOrStricter
  ) where

import           Data.Semigroup                     ((<>))
import           GHC.Fingerprint.Type               (Fingerprint)
import           Language.Haskell.TH.Syntax         (Lift (..))

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Base  (Key)
import           Data.Morpheus.Types.Internal.TH    (apply, liftText, liftTextMap)
import           Data.Morpheus.Types.Internal.Value (Value (..))
import qualified Data.Text                          as T (concat)

unKindD :: KindD -> DataTypeKind
unKindD SubscriptionD       = KindObject
unKindD (RegularKindD kind) = kind

isObject :: KindD -> Bool
isObject (RegularKindD KindObject)      = True
isObject (RegularKindD KindInputObject) = True
isObject _                              = False

isInput :: KindD -> Bool
isInput (RegularKindD KindInputObject) = True
isInput _                              = False

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
isFieldNullable = isNullable . fieldTypeWrappers

isNullable :: [WrapperD] -> Bool
isNullable (MaybeD:_) = True
isNullable _          = False

hsToGQLWrapper :: ([WrapperD], (String, [String])) -> ([DataTypeWrapper], (String, [String]))
hsToGQLWrapper (wr, name) = (toGQLWrapper wr, name)

isEqOrStricter :: [WrapperD] -> [WrapperD] -> Bool
isEqOrStricter x y = isEqOrStricterGQL (toGQLWrapper x) (toGQLWrapper y)

isEqOrStricterGQL :: [DataTypeWrapper] -> [DataTypeWrapper] -> Bool
isEqOrStricterGQL [] []                               = True
isEqOrStricterGQL (NonNullType:xs1) (NonNullType:xs2) = isEqOrStricterGQL xs1 xs2
isEqOrStricterGQL (NonNullType:xs1) xs2               = isEqOrStricterGQL xs1 xs2
isEqOrStricterGQL (ListType:xs1) (ListType:xs2)       = isEqOrStricterGQL xs1 xs2
isEqOrStricterGQL _ _                                 = False

toGQLWrapper :: [WrapperD] -> [DataTypeWrapper]
toGQLWrapper (MaybeD:(MaybeD:tw)) = toGQLWrapper (MaybeD : tw)
toGQLWrapper (MaybeD:(ListD:tw))  = ListType : toGQLWrapper tw
toGQLWrapper (ListD:tw)           = [NonNullType, ListType] <> toGQLWrapper tw
toGQLWrapper [MaybeD]             = []
toGQLWrapper []                   = [NonNullType]

toHSWrappers :: [DataTypeWrapper] -> [WrapperD]
toHSWrappers []                             = [MaybeD]
toHSWrappers [NonNullType]                  = []
toHSWrappers (NonNullType:(ListType:xs))    = ListD : toHSWrappers xs
toHSWrappers (NonNullType:(NonNullType:xs)) = toHSWrappers xs
toHSWrappers (ListType:xs)                  = [MaybeD, ListD] <> toHSWrappers xs

data KindD
  = SubscriptionD
  | RegularKindD DataTypeKind
  deriving (Show, Eq, Lift)

data DataTypeKind
  = KindScalar
  | KindObject
  | KindUnion
  | KindEnum
  | KindInputObject
  | KindList
  | KindNonNull
  | KindInputUnion
  deriving (Eq, Show, Lift)

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

data DataField = DataField
  { fieldArgs         :: [(Key, DataArgument)]
  , fieldName         :: Key
  , fieldType         :: Key
  , fieldTypeWrappers :: [WrapperD]
  , fieldHidden       :: Bool
  } deriving (Show)

instance Lift DataField where
  lift (DataField arg nm ty tw hid) = apply 'DataField [liftTextMap arg, liftText nm, liftText ty, lift tw, lift hid]

data DataTyCon a = DataTyCon
  { typeName        :: Key
  , typeFingerprint :: DataFingerprint
  , typeDescription :: Key
  , typeVisibility  :: Bool
  , typeData        :: a
  } deriving (Show)

data DataLeaf
  = BaseScalar DataScalar
  | CustomScalar DataScalar
  | LeafEnum DataEnum
  deriving (Show)

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

showFullAstType :: [WrapperD] -> DataKind -> Key
showFullAstType wrappers' (ScalarKind x) = showWrappedType wrappers' (typeName x)
showFullAstType wrappers' (EnumKind x)   = showWrappedType wrappers' (typeName x)
showFullAstType wrappers' (ObjectKind x) = showWrappedType wrappers' (typeName x)
showFullAstType wrappers' (UnionKind x)  = showWrappedType wrappers' (typeName x)

showWrappedType :: [WrapperD] -> Key -> Key
showWrappedType wr = showGQLWrapper (toGQLWrapper wr)

showGQLWrapper :: [DataTypeWrapper] -> Key -> Key
showGQLWrapper [] type'               = type'
showGQLWrapper (ListType:xs) type'    = T.concat ["[", showGQLWrapper xs type', "]"]
showGQLWrapper (NonNullType:xs) type' = T.concat [showGQLWrapper xs type', "!"]

initTypeLib :: (Key, DataObject) -> DataTypeLib
initTypeLib query' =
  DataTypeLib
    { leaf = []
    , inputObject = []
    , query = query'
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
kindOf (OutputObject _)        = KindObject
kindOf (Union _)               = KindUnion
kindOf (InputUnion _)          = KindInputUnion

isTypeDefined :: Key -> DataTypeLib -> Maybe DataFingerprint
isTypeDefined name lib = getTypeFingerprint <$> lookupDataType name lib
  where
    getTypeFingerprint :: DataFullType -> DataFingerprint
    getTypeFingerprint (Leaf (BaseScalar dataType'))   = typeFingerprint dataType'
    getTypeFingerprint (Leaf (CustomScalar dataType')) = typeFingerprint dataType'
    getTypeFingerprint (Leaf (LeafEnum dataType'))     = typeFingerprint dataType'
    getTypeFingerprint (InputObject dataType')         = typeFingerprint dataType'
    getTypeFingerprint (OutputObject dataType')        = typeFingerprint dataType'
    getTypeFingerprint (Union dataType')               = typeFingerprint dataType'
    getTypeFingerprint (InputUnion dataType')          = typeFingerprint dataType'

defineType :: (Key, DataFullType) -> DataTypeLib -> DataTypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
defineType (key', InputUnion type') lib   = lib {inputUnion = (key', type') : inputUnion lib}

toNullableField :: DataField -> DataField
toNullableField dataField@DataField {fieldTypeWrappers = MaybeD:xs} = dataField {fieldTypeWrappers = xs}
toNullableField dataField                                           = dataField

toListField :: DataField -> DataField
toListField x = x {fieldTypeWrappers = ListD : fieldTypeWrappers x}
