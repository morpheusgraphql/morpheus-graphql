module Data.Morpheus.Schema.Internal.Types
  ( OutputType
  , InternalType(..)
  , Core(..)
  , Field(..)
  , ObjectField(..)
  , InputType
  , InputField(..)
  , TypeLib(..)
  , GType(..)
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
  }

data Core = Core
  { name            :: Text
  , typeDescription :: Text
  }

data Field = Field
  { fieldName :: Text
  , notNull   :: Bool
  , kind      :: TypeKind
  , fieldType :: Text
  }

data ObjectField = ObjectField
  { args         :: [(Text, InputField)]
  , fieldContent :: Field
  }

data GObject a =
  GObject [(Text, a)]
          Core

data InternalType a
  = Scalar Core
  | Enum [EnumValue]
         Core
  | Object (GObject a)

type OutputType = InternalType ObjectField

type InputType = InternalType InputField

type InputObject = GObject InputField

type OutputObject = GObject ObjectField

data GType
  = OType OutputType
  | IType InputType

data Leaf
  = LScalar Core
  | LEnum [EnumValue]
          Core

data TypeLib = TypeLib
  { leaf        :: [(Text, Leaf)]
  , inputObject :: [(Text, InputObject)]
  , object      :: [(Text, OutputObject)]
  }

initTypeLib :: TypeLib
initTypeLib = TypeLib {leaf = [], inputObject = [], object = []}

data LibType
  = Leaf Leaf
  | InputObject InputObject
  | OutputObject OutputObject

getAllTypeKeys :: TypeLib -> [Text]
getAllTypeKeys (TypeLib leaf' inputObject' object') = map fst leaf' ++ map fst inputObject' ++ map fst object'

isTypeDefined :: Text -> TypeLib -> Bool
isTypeDefined name' lib' = name' `elem` getAllTypeKeys lib'

defineType :: (Text, LibType) -> TypeLib -> TypeLib
defineType (key', Leaf type') lib         = lib {leaf = [(key', type')] ++ leaf lib}
defineType (key', InputObject type') lib  = lib {inputObject = [(key', type')] ++ inputObject lib}
defineType (key', OutputObject type') lib = lib {object = [(key', type')] ++ object lib}
