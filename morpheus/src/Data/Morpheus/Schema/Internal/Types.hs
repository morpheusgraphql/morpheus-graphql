module Data.Morpheus.Schema.Internal.Types
  ( OutputType(..)
  , Core(..)
  , Field(..)
  , ObjectField(..)
  , InputType(..)
  , InputField(..)
  , TypeLib
  , GType(..)
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
  , typeName  :: Text
  }

data ObjectField =
  ObjectField [InputField]
              Field

data OutputType
  = Scalar Core
  | Enum [EnumValue]
         Core
  | Object [ObjectField]
           Core

data InputType
  = IScalar Core
  | IEnum [EnumValue]
          Core
  | IObject [Field]
            Core

data GType
  = OType OutputType
  | IType InputType

type TypeLib = Map Text GType
