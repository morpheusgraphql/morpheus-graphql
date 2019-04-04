module Data.Morpheus.PreProcess.Utils
  ( fieldOf
  , differKeys
  , existsObjectType
  , lookupType
  , getInputType
  ) where

import           Data.List                           ((\\))
import           Data.Morpheus.Schema.Internal.Types (InputType, InternalType (..), Leaf (..), OutputObject,
                                                      TypeLib (..))
import           Data.Morpheus.Types.Core            (EnhancedKey (..), Key, enhanceKeyWithNull)
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           (Text)

lookupType :: error -> [(Text, a)] -> Text -> Either error a
lookupType error' lib' typeName' =
  case lookup typeName' lib' of
    Nothing -> Left error'
    Just x  -> pure x

existsTypeIn :: (Position, Key) -> [(Text, a)] -> Text -> MetaValidation a
existsTypeIn (position', key') lib typeName' = lookupType (UnknownType meta) lib typeName'
  where
    meta = MetaInfo {position = position', typeName = typeName', key = key'}

getInputType :: (Position, Key) -> Text -> TypeLib -> MetaValidation InputType
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

existsObjectType :: (Position, Key) -> Text -> TypeLib -> MetaValidation OutputObject
existsObjectType (position', key') typeName' lib = existsTypeIn (position', key') (object lib) typeName'

fieldOf :: (Position, Text) -> [(Text, fType)] -> Text -> MetaValidation fType
fieldOf (pos, tName) outType fName =
  case lookup fName outType of
    Nothing    -> Left $ UnknownField $ MetaInfo {key = fName, typeName = tName, position = pos}
    Just field -> pure field

differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys
