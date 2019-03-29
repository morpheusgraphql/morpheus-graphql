module Data.Morpheus.Schema.Internal.Types
  ( Type(..)
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
  { name        :: Text
  , description :: Text
  , typeID      :: Text
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

data Type
  = Scalar Type
  | Enum Type
         [EnumValue]
  | Object Type
           [ObjectField]

data InputType
  = IScalar Type
  | IEnum Type
          [EnumValue]
  | IObject Type
            [Field]
