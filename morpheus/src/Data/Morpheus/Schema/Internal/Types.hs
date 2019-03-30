module Data.Morpheus.Schema.Internal.Types
  ( OutputType(..)
  , Core(..)
  , Field(..)
  , ObjectField(..)
  , InputType(..)
  , InputField(..)
  , TypeLib
  , GType(..)
  , ObjectFields
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

type ObjectFields = [(Text, ObjectField)]

data OutputType
  = Scalar Core
  | Enum [EnumValue]
         Core
  | Object ObjectFields
           Core

data InputType
  = IScalar Core
  | IEnum [EnumValue]
          Core
  | IObject [InputField]
            Core

data GType
  = OType OutputType
  | IType InputType

type TypeLib = Map Text GType
