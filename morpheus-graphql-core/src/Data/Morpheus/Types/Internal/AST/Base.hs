{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Base
  ( Ref (..),
    Position (..),
    Message (..),
    FieldName (..),
    Description,
    TypeWrapper (..),
    TypeRef (..),
    VALIDATION_MODE (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    TypeKind (..),
    DataFingerprint (..),
    DataTypeWrapper (..),
    Token,
    anonymousRef,
    toHSWrappers,
    toGQLWrapper,
    Nullable (..),
    isWeaker,
    isSubscription,
    isOutputObject,
    isNotSystemTypeName,
    isObject,
    isInput,
    sysFields,
    hsTypeName,
    toOperationType,
    splitDuplicates,
    removeDuplicates,
    GQLError (..),
    GQLErrors,
    internalFingerprint,
    TRUE,
    FALSE,
    TypeName (..),
    Msg (..),
    intercalateName,
    toFieldName,
    TypeNameRef (..),
    convertToJSONName,
    convertToHaskellName,
    isOutput,
    mkTypeRef,
    InternalError (..),
    msgInternal,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value,
    encode,
  )
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Char (toLower)
import Data.Hashable (Hashable)
import Data.Morpheus.Rendering.RenderGQL (RenderGQL (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString)
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Haskell.TH
  ( ExpQ,
    stringE,
  )
import Language.Haskell.TH.Syntax
  ( Lift (..),
    Q,
    TExp,
    unsafeTExpCoerce,
  )
import Prelude
  ( ($),
    (&&),
    (.),
    Bool (..),
    Eq (..),
    Functor (..),
    Int,
    Maybe (..),
    Ord (..),
    Show (..),
    String,
    elem,
    fst,
    id,
    not,
    notElem,
    otherwise,
  )

type TRUE = 'True

type FALSE = 'False

-- Strings
type Token = Text

-- Error / Warning Messages
newtype Message = Message {readMessage :: Text}
  deriving
    (Generic)
  deriving newtype
    (Show, Eq, Ord, IsString, Semigroup, Hashable, FromJSON, ToJSON)

instance Lift Message where
  lift = liftString . readMessage

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readMessage
#endif

newtype InternalError = InternalError
  { readInternalError :: Text
  }
  deriving
    (Generic)
  deriving newtype
    (Show, Eq, Ord, IsString, Semigroup, Hashable, FromJSON, ToJSON)

instance Lift InternalError where
  lift = liftString . readInternalError

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readInternalError
#endif

msgInternal :: (Msg a) => a -> InternalError
msgInternal = InternalError . readMessage . msg

class Msg a where
  msg :: a -> Message
  msgSepBy :: Text -> [a] -> Message
  msgSepBy t = Message . intercalate t . fmap (readMessage . msg)

instance Msg Message where
  msg = id

instance Msg InternalError where
  msg = Message . ("Internal Error! " <>) . readInternalError

instance Msg String where
  msg = Message . pack

instance Msg ByteString where
  msg = msg . unpack

instance Msg Text where
  msg = Message

instance Msg Value where
  msg = msg . encode

class Nullable a where
  isNullable :: a -> Bool
  toNullable :: a -> a

-- FieldName : lower case names
newtype FieldName = FieldName {readName :: Text}
  deriving
    (Generic)
  deriving newtype
    (Show, Ord, Eq, IsString, Hashable, Semigroup, FromJSON, ToJSON)

instance Lift FieldName where
  lift = liftString . readName

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readName
#endif

instance Msg FieldName where
  msg FieldName {readName} = Message $ "\"" <> readName <> "\""

instance RenderGQL FieldName where
  render = readName

intercalateName :: FieldName -> [FieldName] -> FieldName
intercalateName (FieldName x) = FieldName . intercalate x . fmap readName

toFieldName :: TypeName -> FieldName
toFieldName = FieldName . readTypeName

-- TypeName
newtype TypeName = TypeName {readTypeName :: Text}
  deriving
    (Generic)
  deriving newtype
    (Show, Ord, Eq, IsString, Hashable, Semigroup, FromJSON, ToJSON)

instance Lift TypeName where
  lift = liftString . readTypeName

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readTypeName
#endif

liftTypedString :: IsString a => Token -> Q (TExp a)
liftTypedString = unsafeTExpCoerce . stringE . T.unpack

liftString :: Token -> ExpQ
liftString = stringE . T.unpack

instance Msg TypeName where
  msg TypeName {readTypeName} = Message $ "\"" <> readTypeName <> "\""

instance RenderGQL TypeName where
  render = readTypeName

-- Description
type Description = Text

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON, Lift)

-- Positions 2 Value withs same structire
-- but different Positions should be Equal
instance Eq Position where
  _ == _ = True

data GQLError = GQLError
  { message :: Message,
    locations :: [Position]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

type GQLErrors = [GQLError]

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

data DataFingerprint = DataFingerprint TypeName [String] deriving (Show, Eq, Ord, Lift)

internalFingerprint :: TypeName -> [String] -> DataFingerprint
internalFingerprint name = DataFingerprint ("SYSTEM.INTERNAL." <> name)

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Lift, Generic, Hashable)

instance RenderGQL OperationType where
  render = pack . fmap toLower . show

instance Msg OperationType where
  msg Query = msg ("query" :: TypeName)
  msg Mutation = msg ("mutation" :: TypeName)
  msg Subscription = msg ("subscription" :: TypeName)

type QUERY = 'Query

type MUTATION = 'Mutation

type SUBSCRIPTION = 'Subscription

data TypeNameRef = TypeNameRef
  { typeNameRef :: TypeName,
    typeNamePosition :: Position
  }
  deriving (Show, Lift, Eq)

-- Refference with Position information
--
-- includes position for debugging, where Ref "a" 1 === Ref "a" 3
--
data Ref = Ref
  { refName :: FieldName,
    refPosition :: Position
  }
  deriving (Show, Lift, Eq)

instance Ord Ref where
  compare (Ref x _) (Ref y _) = compare x y

anonymousRef :: FieldName -> Ref
anonymousRef refName = Ref {refName, refPosition = Position 0 0}

-- TypeRef
-------------------------------------------------------------------
data TypeRef = TypeRef
  { typeConName :: TypeName,
    typeArgs :: Maybe TypeName,
    typeWrappers :: [TypeWrapper]
  }
  deriving (Show, Eq, Lift)

mkTypeRef :: TypeName -> TypeRef
mkTypeRef typeConName =
  TypeRef {typeConName, typeWrappers = [], typeArgs = Nothing}

instance Nullable TypeRef where
  isNullable = isNullable . typeWrappers
  toNullable TypeRef {..} = TypeRef {typeWrappers = toNullable typeWrappers, ..}

instance RenderGQL TypeRef where
  render TypeRef {typeConName, typeWrappers} = renderWrapped typeConName typeWrappers

instance Msg TypeRef where
  msg = msg . FieldName . render

-- Kind
-----------------------------------------------------------------------------------
data TypeKind
  = KindScalar
  | KindObject (Maybe OperationType)
  | KindUnion
  | KindEnum
  | KindInputObject
  | KindList
  | KindNonNull
  | KindInputUnion
  | KindInterface
  deriving (Eq, Show, Lift)

instance RenderGQL TypeKind where
  render KindScalar = "SCALAR"
  render KindObject {} = "OBJECT"
  render KindUnion = "UNION"
  render KindInputUnion = "INPUT_OBJECT"
  render KindEnum = "ENUM"
  render KindInputObject = "INPUT_OBJECT"
  render KindList = "LIST"
  render KindNonNull = "NON_NULL"
  render KindInterface = "INTERFACE"

isSubscription :: TypeKind -> Bool
isSubscription (KindObject (Just Subscription)) = True
isSubscription _ = False

isOutputObject :: TypeKind -> Bool
isOutputObject (KindObject _) = True
isOutputObject KindInterface = True
isOutputObject _ = False

isOutput :: TypeKind -> Bool
isOutput (KindObject _) = True
isOutput KindUnion = True
isOutput KindInterface = True
isOutput _ = False

isObject :: TypeKind -> Bool
isObject (KindObject _) = True
isObject KindInputObject = True
isObject KindInterface = True
isObject _ = False

isInput :: TypeKind -> Bool
isInput KindInputObject = True
isInput _ = False

-- TypeWrappers
-----------------------------------------------------------------------------------
data TypeWrapper
  = TypeList
  | TypeMaybe
  deriving (Show, Eq, Lift)

data DataTypeWrapper
  = ListType
  | NonNullType
  deriving (Show, Lift)

instance Nullable [TypeWrapper] where
  isNullable (TypeMaybe : _) = True
  isNullable _ = False
  toNullable (TypeMaybe : xs) = TypeMaybe : xs
  toNullable xs = TypeMaybe : xs

isWeaker :: [TypeWrapper] -> [TypeWrapper] -> Bool
isWeaker (TypeMaybe : xs1) (TypeMaybe : xs2) = isWeaker xs1 xs2
isWeaker (TypeMaybe : _) _ = True
isWeaker (_ : xs1) (_ : xs2) = isWeaker xs1 xs2
isWeaker _ _ = False

toGQLWrapper :: [TypeWrapper] -> [DataTypeWrapper]
toGQLWrapper (TypeMaybe : (TypeMaybe : tw)) = toGQLWrapper (TypeMaybe : tw)
toGQLWrapper (TypeMaybe : (TypeList : tw)) = ListType : toGQLWrapper tw
toGQLWrapper (TypeList : tw) = [NonNullType, ListType] <> toGQLWrapper tw
toGQLWrapper [TypeMaybe] = []
toGQLWrapper [] = [NonNullType]

toHSWrappers :: [DataTypeWrapper] -> [TypeWrapper]
toHSWrappers (NonNullType : (NonNullType : xs)) =
  toHSWrappers (NonNullType : xs)
toHSWrappers (NonNullType : (ListType : xs)) = TypeList : toHSWrappers xs
toHSWrappers (ListType : xs) = [TypeMaybe, TypeList] <> toHSWrappers xs
toHSWrappers [] = [TypeMaybe]
toHSWrappers [NonNullType] = []

renderWrapped :: RenderGQL a => a -> [TypeWrapper] -> Token
renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
  where
    showGQLWrapper [] = render x
    showGQLWrapper (ListType : xs) = "[" <> showGQLWrapper xs <> "]"
    showGQLWrapper (NonNullType : xs) = showGQLWrapper xs <> "!"

isNotSystemTypeName :: TypeName -> Bool
isNotSystemTypeName =
  ( `notElem`
      [ "__Schema",
        "__Type",
        "__Directive",
        "__TypeKind",
        "__Field",
        "__DirectiveLocation",
        "__InputValue",
        "__EnumValue",
        "String",
        "Float",
        "Int",
        "Boolean",
        "ID"
      ]
  )

sysFields :: [FieldName]
sysFields = ["__typename", "__schema", "__type"]

hsTypeName :: TypeName -> TypeName
hsTypeName "String" = "Text"
hsTypeName "Boolean" = "Bool"
hsTypeName name = name

toOperationType :: TypeName -> Maybe OperationType
toOperationType "Subscription" = Just Subscription
toOperationType "Mutation" = Just Mutation
toOperationType "Query" = Just Query
toOperationType _ = Nothing

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = fst . splitDuplicates

-- elems -> (unique elements, duplicate elems)
splitDuplicates :: Eq a => [a] -> ([a], [a])
splitDuplicates = collectElems ([], [])
  where
    collectElems :: Eq a => ([a], [a]) -> [a] -> ([a], [a])
    collectElems collected [] = collected
    collectElems (collected, errors) (x : xs)
      | x `elem` collected = collectElems (collected, errors <> [x]) xs
      | otherwise = collectElems (collected <> [x], errors) xs

-- handle reserved Names
isReserved :: FieldName -> Bool
isReserved "case" = True
isReserved "class" = True
isReserved "data" = True
isReserved "default" = True
isReserved "deriving" = True
isReserved "do" = True
isReserved "else" = True
isReserved "foreign" = True
isReserved "if" = True
isReserved "import" = True
isReserved "in" = True
isReserved "infix" = True
isReserved "infixl" = True
isReserved "infixr" = True
isReserved "instance" = True
isReserved "let" = True
isReserved "module" = True
isReserved "newtype" = True
isReserved "of" = True
isReserved "then" = True
isReserved "type" = True
isReserved "where" = True
isReserved "_" = True
isReserved _ = False
{-# INLINE isReserved #-}

convertToJSONName :: FieldName -> FieldName
convertToJSONName (FieldName hsName)
  | not (T.null hsName) && isReserved (FieldName name) && (T.last hsName == '\'') = FieldName name
  | otherwise = FieldName hsName
  where
    name = T.init hsName

convertToHaskellName :: FieldName -> FieldName
convertToHaskellName name
  | isReserved name = name <> "'"
  | otherwise = name
