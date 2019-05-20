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
  , isTypeDefined
  , initTypeLib
  , defineType
  , showWrappedType
  , showFullAstType
  , isFieldNullable
  ) where

import           Data.Morpheus.Schema.TypeKind (TypeKind)
import           Data.Text                     (Text)
import qualified Data.Text                     as T (concat)

type Key = Text

type DataScalar = DataType ()

type DataEnum = DataType [Key]

type DataObject a = DataType [(Key, a)]

type DataInputField = DataField ()

type DataArgument = DataInputField

type DataOutputField = DataField [(Key, DataArgument)]

type DataInputObject = DataObject DataInputField

type DataOutputObject = DataObject DataOutputField

type DataUnion = [DataField ()]

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

mutationName :: Maybe (Text, DataOutputObject) -> [Text]
mutationName (Just (key', _)) = [key']
mutationName Nothing          = []

subscriptionName :: Maybe (Text, DataOutputObject) -> [Text]
subscriptionName (Just (key', _)) = [key']
subscriptionName Nothing          = []

getAllTypeKeys :: DataTypeLib -> [Text]
getAllTypeKeys (DataTypeLib leaf' inputObject' object' union' (queryName, _) mutation' subscription') =
  [queryName] ++
  map fst leaf' ++
  map fst inputObject' ++ map fst object' ++ mutationName mutation' ++ subscriptionName subscription' ++ map fst union'

isTypeDefined :: Text -> DataTypeLib -> Bool
isTypeDefined name' lib' = name' `elem` getAllTypeKeys lib'

defineType :: (Text, DataFullType) -> DataTypeLib -> DataTypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
