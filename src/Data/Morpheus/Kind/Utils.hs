module Data.Morpheus.Kind.Utils
  ( maybeField
  , listField
  , encodeMaybe
  , encodeList
  ) where

import           Data.Morpheus.Types.Error           (ResolveIO)
import           Data.Morpheus.Types.Internal.AST    (ASTField (..))
import           Data.Morpheus.Types.Internal.Value  (Value (..))
import           Data.Morpheus.Types.Query.Operator  (TypeWrapper (..))
import           Data.Morpheus.Types.Query.Selection (Selection (..))
import           Data.Text                           (Text)

maybeField :: ASTField a -> ASTField a
maybeField field@ASTField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
maybeField field                                               = field

listField :: ASTField a -> ASTField a
listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}

type Encode a = (Text, Selection) -> a -> ResolveIO Value

encodeList :: Encode a -> Encode [a]
encodeList _ (_, Field {}) _ = pure $ JSList []
encodeList f query list      = JSList <$> mapM (f query) list

encodeMaybe :: Encode a -> Encode (Maybe a)
encodeMaybe _ _ Nothing          = pure JSNull
encodeMaybe f query (Just value) = f query value
