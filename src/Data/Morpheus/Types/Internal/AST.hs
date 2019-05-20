{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Internal.AST
  ( Key
  , ASTScalar
  , ASTEnum
  , ASTObject
  , ASTInputField
  , ASTArgument
  , ASTOutputField
  , ASTInputObject
  , ASTOutputObject
  , ASTUnion
  , ASTOutputType
  , ASTInputType
  , ASTField(..)
  , ASTType(..)
  , ASTLeaf(..)
  , ASTKind(..)
  , ASTFullType(..)
  , ASTTypeLib(..)
  , isTypeDefined
  , initTypeLib
  , defineType
  , showWrappedType
  , showFullAstType
  , isFieldNullable
  ) where

import           Data.Morpheus.Schema.TypeKind      (TypeKind)
import           Data.Morpheus.Types.Query.Operator (TypeWrapper (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T (concat)

type Key = Text

type ASTScalar = ASTType ()

type ASTEnum = ASTType [Key]

type ASTObject a = ASTType [(Key, a)]

type ASTInputField = ASTField ()

type ASTArgument = ASTInputField

type ASTOutputField = ASTField [(Key, ASTArgument)]

type ASTInputObject = ASTObject ASTInputField

type ASTOutputObject = ASTObject ASTOutputField

type ASTUnion = [ASTField ()]

type ASTOutputType = ASTKind ASTOutputField

type ASTInputType = ASTKind ASTInputField

data ASTField a = ASTField
  { fieldArgs         :: a
  , fieldName         :: Text
  , fieldKind         :: TypeKind
  , fieldType         :: Text
  , fieldTypeWrappers :: [TypeWrapper]
  } deriving (Show)

isFieldNullable :: ASTField a -> Bool
isFieldNullable ASTField {fieldTypeWrappers = NonNullType:_} = False
isFieldNullable _                                            = True

data ASTType a = ASTType
  { typeName        :: Text
  , typeDescription :: Text
  , typeData        :: a
  } deriving (Show)

data ASTLeaf
  = LeafScalar ASTScalar
  | LeafEnum ASTEnum
  deriving (Show)

data ASTKind a
  = Scalar ASTScalar
  | Enum ASTEnum
  | Object (ASTObject a)
  deriving (Show)

data ASTFullType
  = Leaf ASTLeaf
  | InputObject ASTInputObject
  | OutputObject ASTOutputObject
  | Union ASTUnion
  deriving (Show)

data ASTTypeLib = ASTTypeLib
  { leaf         :: [(Text, ASTLeaf)]
  , inputObject  :: [(Text, ASTInputObject)]
  , object       :: [(Text, ASTOutputObject)]
  , union        :: [(Text, ASTUnion)]
  , query        :: (Text, ASTOutputObject)
  , mutation     :: Maybe (Text, ASTOutputObject)
  , subscription :: Maybe (Text, ASTOutputObject)
  }

showWrappedType :: [TypeWrapper] -> Text -> Text
showWrappedType [] type'               = type'
showWrappedType (ListType:xs) type'    = T.concat ["[", showWrappedType xs type', "]"]
showWrappedType (NonNullType:xs) type' = T.concat [showWrappedType xs type', "!"]

showFullAstType :: [TypeWrapper] -> ASTType a -> Text
showFullAstType wrappers' = showWrappedType wrappers' . typeName

initTypeLib :: (Text, ASTOutputObject) -> ASTTypeLib
initTypeLib query' =
  ASTTypeLib
    {leaf = [], inputObject = [], query = query', object = [], union = [], mutation = Nothing, subscription = Nothing}

mutationName :: Maybe (Text, ASTOutputObject) -> [Text]
mutationName (Just (key', _)) = [key']
mutationName Nothing          = []

subscriptionName :: Maybe (Text, ASTOutputObject) -> [Text]
subscriptionName (Just (key', _)) = [key']
subscriptionName Nothing          = []

getAllTypeKeys :: ASTTypeLib -> [Text]
getAllTypeKeys (ASTTypeLib leaf' inputObject' object' union' (queryName, _) mutation' subscription') =
  [queryName] ++
  map fst leaf' ++
  map fst inputObject' ++ map fst object' ++ mutationName mutation' ++ subscriptionName subscription' ++ map fst union'

isTypeDefined :: Text -> ASTTypeLib -> Bool
isTypeDefined name' lib' = name' `elem` getAllTypeKeys lib'

defineType :: (Text, ASTFullType) -> ASTTypeLib -> ASTTypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
