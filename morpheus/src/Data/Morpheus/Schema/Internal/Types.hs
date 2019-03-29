module Data.Morpheus.Schema.Internal.Types
  ( OutputType(..)
  , Core(..)
  , Field(..)
  , ObjectField(..)
  , InputType(..)
  , Arg
  ) where

import           Data.Morpheus.Schema.TypeKind (TypeKind)
import           Data.Text                     (Text)

type EnumValue = Text

type Arg = Core

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
  ObjectField [Arg]
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
