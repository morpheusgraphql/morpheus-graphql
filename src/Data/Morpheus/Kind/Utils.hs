module Data.Morpheus.Kind.Utils
  ( maybeField
  , listField
  , encodeMaybe
  , encodeList
  ) where

import           Data.Morpheus.Kind.GQLKinds                (Encode_)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Value         (Value (..))

maybeField :: DataField a -> DataField a
maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
maybeField field                                                = field

listField :: DataField a -> DataField a
listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}

encodeList :: Encode_ a -> Encode_ [a]
encodeList _ (_, Selection {selectionRec = SelectionField {}}) _ = pure $ List []
encodeList f query list                                          = List <$> mapM (f query) list

encodeMaybe :: Encode_ a -> Encode_ (Maybe a)
encodeMaybe _ _ Nothing          = pure Null
encodeMaybe f query (Just value) = f query value
