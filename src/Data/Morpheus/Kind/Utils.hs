module Data.Morpheus.Kind.Utils
  ( maybeField
  , listField
  , maybeInputField
  , listInputField
  , encodeMaybe
  , encodeList
  ) where

import           Data.Morpheus.Schema.Internal.AST   (InputField (..), ObjectField (..))
import qualified Data.Morpheus.Schema.Internal.AST   as I (Field (..))
import           Data.Morpheus.Types.Error           (ResolveIO)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.Query.Operator  (ListWrapper (..))
import           Data.Morpheus.Types.Query.Selection (Selection (..))
import           Data.Text                           (Text)

setNull :: I.Field -> I.Field
setNull x = x {I.notNull = False}

setList :: I.Field -> I.Field
setList x = x {I.fieldTypeWrappers = ListWrapper False : I.fieldTypeWrappers x}

maybeInputField :: InputField -> InputField
maybeInputField = InputField . setNull . unpackInputField

listInputField :: InputField -> InputField
listInputField = InputField . setList . unpackInputField

maybeField :: ObjectField -> ObjectField
maybeField x = x {fieldContent = setNull (fieldContent x)}

listField :: ObjectField -> ObjectField
listField x = x {fieldContent = setList (fieldContent x)}

type Encode a = (Text, Selection) -> a -> ResolveIO JSType

encodeList :: Encode a -> Encode [a]
encodeList _ (_, Field {}) _ = pure $ JSList []
encodeList f query list      = JSList <$> mapM (f query) list

encodeMaybe :: Encode a -> Encode (Maybe a)
encodeMaybe _ _ Nothing          = pure JSNull
encodeMaybe f query (Just value) = f query value
