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

import           Data.Morpheus.Schema.TypeKind (TypeKind)
import           Data.Text                     (Text)

type EnumValue = Text

newtype InputField = InputField
  { unpackInputField :: Field
  } deriving (Show)

data Core = Core
  { name            :: Text
  , typeDescription :: Text
  } deriving (Show)

data Field = Field
  { fieldName :: Text
  , notNull   :: Bool
  , kind      :: TypeKind
  , fieldType :: Text
  , asList    :: Bool
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
  { leaf        :: [(Text, Leaf)]
  , inputObject :: [(Text, InputObject)]
  , object      :: [(Text, OutputObject)]
  , query       :: (Text, OutputObject)
  , mutation    :: Maybe (Text, OutputObject)
  , union       :: [(Text, [Field])]
  }

initTypeLib :: (Text, OutputObject) -> TypeLib
initTypeLib query' = TypeLib {leaf = [], inputObject = [], query = query', object = [], mutation = Nothing, union = []}

data LibType
  = Leaf Leaf
  | InputObject InputObject
  | OutputObject OutputObject
  | Union [Field]
  deriving (Show)

mutationName :: Maybe (Text, OutputObject) -> [Text]
mutationName (Just (key', _)) = [key']
mutationName Nothing          = []

getAllTypeKeys :: TypeLib -> [Text]
getAllTypeKeys (TypeLib leaf' inputObject' object' (queryName, _) mutation' union') =
  [queryName] ++ map fst leaf' ++ map fst inputObject' ++ map fst object' ++ mutationName mutation' ++ map fst union'

isTypeDefined :: Text -> TypeLib -> Bool
isTypeDefined name' lib' = name' `elem` getAllTypeKeys lib'

defineType :: (Text, LibType) -> TypeLib -> TypeLib
defineType (key', Leaf type') lib         = lib {leaf = (key', type') : leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = (key', type') : inputObject lib}
defineType (key', OutputObject type') lib = lib {object = (key', type') : object lib}
defineType (key', Union type') lib        = lib {union = (key', type') : union lib}
