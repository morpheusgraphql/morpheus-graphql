module Data.Morpheus.Schema.Internal.Types
  ( OutputType
  , InternalType(..)
  , Core(..)
  , Field(..)
  , ObjectField(..)
  , InputType
  , InputField(..)
  , TypeLib
  , GType(..)
  , GObject(..)
  ) where

import           Data.Map                      (Map)
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
  { args         :: [InputField]
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

data GType
  = OType OutputType
  | IType InputType

type TypeLib = Map Text GType
