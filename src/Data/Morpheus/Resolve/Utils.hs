module Data.Morpheus.Resolve.Utils
  ( maybeField
  , listField
  ) where

import           Data.Morpheus.Types.Internal.Data (DataField (..), DataTypeWrapper (..))

maybeField :: DataField a -> DataField a
maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
maybeField field                                                = field

listField :: DataField a -> DataField a
listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
