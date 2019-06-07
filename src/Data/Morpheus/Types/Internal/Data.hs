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
  , isTypeDefined
  , initTypeLib
  , defineType
  , showWrappedType
  , showFullAstType
  , isFieldNullable
  ) where

import           Data.Morpheus.Schema.TypeKind      (TypeKind)
import           Data.Morpheus.Types.Internal.Value (Value (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T (concat)

type Key = Text

type TypeID = Text

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
  } deriving (Show)

isFieldNullable :: DataField a -> Bool
isFieldNullable DataField {fieldTypeWrappers = NonNullType:_} = False
isFieldNullable _                                             = True

data DataType a = DataType
  { typeName        :: Text
  , typeHash        :: TypeID
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

maybeDataType :: Maybe (Text, DataOutputObject) -> [(Text, TypeID)]
maybeDataType (Just (key', dataType')) = [(key', typeHash dataType')]
maybeDataType Nothing                  = []

typeIdentity :: (Text, DataType a) -> (Text, TypeID)
typeIdentity (name', dataType') = (name', typeHash dataType')

typeIdentityLeaf :: (Text, DataLeaf) -> (Text, TypeID)
typeIdentityLeaf (name', LeafScalar dataType') = (name', typeHash dataType')
typeIdentityLeaf (name', LeafEnum dataType')   = (name', typeHash dataType')

getAllTypeKeys :: DataTypeLib -> [(Text, TypeID)]
getAllTypeKeys (DataTypeLib leaf' inputObject' object' union' query' mutation' subscription') =
  typeIdentity query' :
  map typeIdentity inputObject' ++
  map typeIdentity object' ++
  maybeDataType mutation' ++ maybeDataType subscription' ++ map typeIdentity union' ++ map typeIdentityLeaf leaf'

isTypeDefined :: Text -> DataTypeLib -> Maybe TypeID
isTypeDefined name' lib' = name' `lookup` getAllTypeKeys lib'

defineType :: (Text, DataFullType) -> DataTypeLib -> DataTypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
