module Data.Morpheus.Kind.Utils
  (listField, nullableField, encodeList, encodeMaybe)where

import           Data.Morpheus.Schema.Internal.Types (ObjectField (..))
import qualified Data.Morpheus.Schema.Internal.Types as I (Field (..))
import           Data.Morpheus.Types.Error           (ResolveIO)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.Query.Selection (Selection (..))
import           Data.Text                           (Text)

listField :: ObjectField -> ObjectField
listField x = x {fieldContent = (fieldContent x) {I.notNull = False}}

nullableField :: ObjectField -> ObjectField
nullableField x = x {fieldContent = (fieldContent x) {I.asList = True}}

type Encode a = (Text, Selection) -> a -> ResolveIO JSType

encodeList :: Encode a -> Encode [a]
encodeList _ (_, Field {}) _ = pure $ JSList []
encodeList f query list      = JSList <$> mapM (f query) list

encodeMaybe :: Encode a -> Encode (Maybe a)
encodeMaybe _ _ Nothing          = pure JSNull
encodeMaybe f query (Just value) = f query value
