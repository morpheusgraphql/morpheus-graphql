{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Kind.Internal
  ( PRIMITIVE
  , SCALAR
  , OBJECT
  , ENUM
  , MAYBE
  , LIST
  , INPUT_OBJECT
  , GQL
  , GQLConstraint
  , IField_
  , OField_
  , Encode_
  , Intro_
  , Decode_
  ) where

import Data.Morpheus.Schema.Internal.Types (InputField, ObjectField, TypeLib)
import Data.Morpheus.Types.Error (ResolveIO, Validation)
import Data.Morpheus.Types.JSType (JSType(..))
import Data.Morpheus.Types.Query.Selection (Selection)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Exts (Constraint)

-- Hidden GQL type System API
type family GQL a :: *

-- default Type Instances
type instance GQL Text = PRIMITIVE

type instance GQL Int = PRIMITIVE

type instance GQL Float = PRIMITIVE

type instance GQL Bool = PRIMITIVE

type instance GQL (Maybe a) = LIST

type instance GQL [a] = MAYBE

-- default Data Kinds
data PRIMITIVE

data SCALAR

data OBJECT

data ENUM

data INPUT_OBJECT

data MAYBE

data LIST

type family GQLConstraint a b :: Constraint

-- class Types class
type Intro_ a = Proxy a -> TypeLib -> TypeLib

type Decode_ a = JSType -> Validation a

type Encode_ a = (Text, Selection) -> a -> ResolveIO JSType

type IField_ a = Proxy a -> Text -> InputField

type OField_ a = Proxy a -> Text -> ObjectField