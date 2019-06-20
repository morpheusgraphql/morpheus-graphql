{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Internal.Data
  ( Key
  , DataScalar
  , DataEnum
  , DataObject
  , DataInputField
  , DataArgument
  , DataOutputField
  , DataInputObject
  , DataOutputObject
  , DataUnion
  , DataOutputType
  , DataInputType
  , DataField(..)
  , DataType(..)
  , DataLeaf(..)
  , DataKind(..)
  , DataFullType(..)
  , DataTypeLib(..)
  , DataTypeWrapper(..)
  , DataValidator(..)
  , DataArguments
  , isTypeDefined
  , initTypeLib
  , defineType
  , showWrappedType
  , showFullAstType
  , isFieldNullable
  , allDataTypes
  ) where

import           Data.Morpheus.Schema.TypeKind      (TypeKind)
import           Data.Morpheus.Types.Internal.Value (Value (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T (concat)
import           GHC.Fingerprint.Type               (Fingerprint)

type Key = Text

newtype DataValidator = DataValidator
  { validateValue :: Value -> Either Text Value
  }

instance Show DataValidator where
  show _ = "DataValidator"

type DataScalar = DataType DataValidator

type DataEnum = DataType [Key]

type DataObject a = DataType [(Key, a)]

type DataInputField = DataField ()

type DataArgument = DataInputField

type DataOutputField = DataField [(Key, DataArgument)]

type DataInputObject = DataObject DataInputField

type DataOutputObject = DataObject DataOutputField

type DataUnion = DataType [DataField ()]

type DataOutputType = DataKind DataOutputField

type DataInputType = DataKind DataInputField

type DataArguments = [(Key, DataArgument)]

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show)

data DataField a = DataField
  { fieldArgs         :: a
  , fieldName         :: Text
  , fieldKind         :: TypeKind
  , fieldType         :: Text
  , fieldTypeWrappers :: [DataTypeWrapper]
  , fieldHidden       :: Bool
  } deriving (Show)

isFieldNullable :: DataField a -> Bool
isFieldNullable DataField {fieldTypeWrappers = NonNullType:_} = False
isFieldNullable _                                             = True

data DataType a = DataType
  { typeName        :: Text
  , typeFingerprint :: Fingerprint
  , typeDescription :: Text
  , typeData        :: a
  } deriving (Show)

data DataLeaf
  = LeafScalar DataScalar
  | LeafEnum DataEnum
  deriving (Show)

data DataKind a
  = ScalarKind DataScalar
  | EnumKind DataEnum
  | ObjectKind (DataObject a)
  deriving (Show)

data DataFullType
  = Leaf DataLeaf
  | InputObject DataInputObject
  | OutputObject DataOutputObject
  | Union DataUnion
  deriving (Show)

data DataTypeLib = DataTypeLib
  { leaf         :: [(Text, DataLeaf)]
  , inputObject  :: [(Text, DataInputObject)]
  , object       :: [(Text, DataOutputObject)]
  , union        :: [(Text, DataUnion)]
  , query        :: (Text, DataOutputObject)
  , mutation     :: Maybe (Text, DataOutputObject)
  , subscription :: Maybe (Text, DataOutputObject)
  }

showWrappedType :: [DataTypeWrapper] -> Text -> Text
showWrappedType [] type'               = type'
showWrappedType (ListType:xs) type'    = T.concat ["[", showWrappedType xs type', "]"]
showWrappedType (NonNullType:xs) type' = T.concat [showWrappedType xs type', "!"]

showFullAstType :: [DataTypeWrapper] -> DataKind a -> Text
showFullAstType wrappers' (ScalarKind x) = showWrappedType wrappers' (typeName x)
showFullAstType wrappers' (EnumKind x)   = showWrappedType wrappers' (typeName x)
showFullAstType wrappers' (ObjectKind x) = showWrappedType wrappers' (typeName x)

initTypeLib :: (Text, DataOutputObject) -> DataTypeLib
initTypeLib query' =
  DataTypeLib
    {leaf = [], inputObject = [], query = query', object = [], union = [], mutation = Nothing, subscription = Nothing}

allDataTypes :: DataTypeLib -> [(Text, DataFullType)]
allDataTypes (DataTypeLib leaf' inputObject' object' union' query' mutation' subscription') =
  packType OutputObject query' :
  map (packType InputObject) inputObject' ++
  map (packType OutputObject) object' ++
  map (packType Leaf) leaf' ++ map (packType Union) union' ++ fromMaybeType mutation' ++ fromMaybeType subscription'
  where
    packType f (x, y) = (x, f y)
    fromMaybeType :: Maybe (Text, DataOutputObject) -> [(Text, DataFullType)]
    fromMaybeType (Just (key', dataType')) = [(key', OutputObject dataType')]
    fromMaybeType Nothing                  = []

isTypeDefined :: Text -> DataTypeLib -> Maybe Fingerprint
isTypeDefined name_ lib' = getTypeFingerprint <$> name_ `lookup` allDataTypes lib'
  where
    getTypeFingerprint :: DataFullType -> Fingerprint
    getTypeFingerprint (Leaf (LeafScalar dataType')) = typeFingerprint dataType'
    getTypeFingerprint (Leaf (LeafEnum dataType'))   = typeFingerprint dataType'
    getTypeFingerprint (InputObject dataType')       = typeFingerprint dataType'
    getTypeFingerprint (OutputObject dataType')      = typeFingerprint dataType'
    getTypeFingerprint (Union dataType')             = typeFingerprint dataType'

defineType :: (Text, DataFullType) -> DataTypeLib -> DataTypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
