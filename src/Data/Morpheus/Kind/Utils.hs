module Data.Morpheus.Kind.Utils
  ( maybeField
  , listField
  , encodeMaybe
  , encodeList
  ) where

import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..))
import           Data.Morpheus.Types.Internal.Data          (DataField (..), DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Text                                  (Text)

maybeField :: DataField a -> DataField a
maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
maybeField field                                                = field

listField :: DataField a -> DataField a
listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}

type Encode a = (Text, Selection) -> a -> ResolveIO Value

encodeList :: Encode a -> Encode [a]
encodeList _ (_, SelectionField {}) _ = pure $ List []
encodeList f query list               = List <$> mapM (f query) list

encodeMaybe :: Encode a -> Encode (Maybe a)
encodeMaybe _ _ Nothing          = pure Null
encodeMaybe f query (Just value) = f query value
