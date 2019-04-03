module Data.Morpheus.PreProcess.Utils
  ( fieldOf
  , differKeys
  , existsObjectType
  , existsTypeIn
  , getInputType
  ) where

import           Data.List                           ((\\))
import           Data.Morpheus.Schema.Internal.Types (InputType, InternalType (..), Leaf (..), OutputObject,
                                                      TypeLib (..))
import           Data.Morpheus.Types.Core            (EnhancedKey (..), Key, enhanceKeyWithNull)
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           as TX (Text)

existsTypeIn :: (Position, Key) -> TX.Text -> [(Text, a)] -> MetaValidation a
existsTypeIn (position', key') typeName' lib =
  case lookup typeName' lib of
    Nothing -> Left $ UnknownType meta
    Just x  -> pure x
  where
    meta = MetaInfo {position = position', typeName = typeName', key = key'}

getInputType :: (Position, Key) -> TX.Text -> TypeLib -> MetaValidation InputType
getInputType (position', key') typeName' lib =
  case lookup typeName' (inputObject lib) of
    Just x -> pure (Object x)
    Nothing ->
      case lookup typeName' (leaf lib) of
        Nothing          -> Left $ UnknownType meta
        Just (LScalar x) -> pure (Scalar x)
        Just (LEnum x y) -> pure (Enum x y)
  where
    meta = MetaInfo {position = position', typeName = typeName', key = key'}

existsObjectType :: (Position, Key) -> TX.Text -> TypeLib -> MetaValidation OutputObject
existsObjectType (position', key') typeName' lib = existsTypeIn (position', key') typeName' (object lib)

fieldOf :: (Position, Text) -> [(Text, fType)] -> Text -> MetaValidation fType
fieldOf (pos, tName) outType fName =
  case lookup fName outType of
    Nothing    -> Left $ UnknownField $ MetaInfo {key = fName, typeName = tName, position = pos}
    Just field -> pure field

differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys
