{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Data.Morpheus.Kind.Internal
  ( SCALAR
  , OBJECT
  , ENUM
  , WRAPPER
  , UNION
  , INPUT_OBJECT
  , KIND
  , GQLConstraint
  , IField_
  , OField_
  , Encode_
  , Intro_
  , Decode_
  ) where

import Data.Morpheus.Types.Describer ((::->))
import Data.Morpheus.Types.Error (ResolveIO, Validation)
import Data.Morpheus.Types.Internal.Data (DataInputField, DataOutputField, DataTypeLib)
import Data.Morpheus.Types.Internal.Value (Value(..))
import Data.Morpheus.Types.Query.Selection (Selection)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Exts (Constraint)

-- Hidden KIND type System API
type family KIND a :: *

-- default Type Instances
type instance KIND Text = SCALAR

type instance KIND Int = SCALAR

type instance KIND Float = SCALAR

type instance KIND Bool = SCALAR

type instance KIND (Maybe a) = WRAPPER

type instance KIND [a] = WRAPPER

type instance KIND (p ::-> a) = WRAPPER

-- default Data Kinds
data SCALAR

data OBJECT

data ENUM

data INPUT_OBJECT

data WRAPPER

data UNION

--data LIST
type family GQLConstraint a b :: Constraint

-- class Types class
type Intro_ a = Proxy a -> DataTypeLib -> DataTypeLib

type Decode_ a = Value -> Validation a

type Encode_ a = (Text, Selection) -> a -> ResolveIO Value

type IField_ a = Proxy a -> Text -> DataInputField

type OField_ a = Proxy a -> Text -> DataOutputField