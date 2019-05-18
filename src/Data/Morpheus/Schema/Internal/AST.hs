module Data.Morpheus.Schema.Internal.AST
  ( OutputType
  , InternalType(..)
  , Core(..)
  , Field(..)
  , ObjectField(..)
  , InputType
  , InputField(..)
  , TypeLib(..)
  , GObject(..)
  , Leaf(..)
  , InputObject
  , OutputObject
  , isTypeDefined
  , initTypeLib
  , defineType
  , LibType(..)
  ) where

import           Data.Morpheus.Schema.TypeKind      (TypeKind)
import           Data.Morpheus.Types.Query.Operator (ListWrapper)
import           Data.Text                          (Text)

type EnumValue = Text

newtype InputField = InputField
  { unpackInputField :: Field
  } deriving (Show)

data Core = Core
  { name            :: Text
  , typeDescription :: Text
  } deriving (Show)

data Field = Field
  { fieldName         :: Text
  , notNull           :: Bool
  , kind              :: TypeKind
  , fieldType         :: Text
  , fieldTypeWrappers :: [ListWrapper]
  } deriving (Show)

data ObjectField = ObjectField
  { args         :: [(Text, InputField)]
  , fieldContent :: Field
  } deriving (Show)

data GObject a =
  GObject [(Text, a)]
          Core
  deriving (Show)

data InternalType a
  = Scalar Core
  | Enum [EnumValue]
         Core
  | Object (GObject a)
  deriving (Show)

type OutputType = InternalType ObjectField

type InputType = InternalType InputField

type InputObject = GObject InputField

type OutputObject = GObject ObjectField

data Leaf
  = LScalar Core
  | LEnum [EnumValue]
          Core
  deriving (Show)

data TypeLib = TypeLib
  { leaf         :: [(Text, Leaf)]
  , inputObject  :: [(Text, InputObject)]
  , object       :: [(Text, OutputObject)]
  , union        :: [(Text, [Field])]
  , query        :: (Text, OutputObject)
  , mutation     :: Maybe (Text, OutputObject)
  , subscription :: Maybe (Text, OutputObject)
  }

initTypeLib :: (Text, OutputObject) -> TypeLib
initTypeLib query' =
  TypeLib
    {leaf = [], inputObject = [], query = query', object = [], union = [], mutation = Nothing, subscription = Nothing}

data LibType
  = Leaf Leaf
  | InputObject InputObject
  | OutputObject OutputObject
  | Union [Field]
  deriving (Show)

mutationName :: Maybe (Text, OutputObject) -> [Text]
mutationName (Just (key', _)) = [key']
mutationName Nothing          = []

subscriptionName :: Maybe (Text, OutputObject) -> [Text]
subscriptionName (Just (key', _)) = [key']
subscriptionName Nothing          = []

getAllTypeKeys :: TypeLib -> [Text]
getAllTypeKeys (TypeLib leaf' inputObject' object' union' (queryName, _) mutation' subscription') =
  [queryName] ++
  map fst leaf' ++
  map fst inputObject' ++ map fst object' ++ mutationName mutation' ++ subscriptionName subscription' ++ map fst union'

isTypeDefined :: Text -> TypeLib -> Bool
isTypeDefined name' lib' = name' `elem` getAllTypeKeys lib'

defineType :: (Text, LibType) -> TypeLib -> TypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
