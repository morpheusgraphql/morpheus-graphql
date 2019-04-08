module Data.Morpheus.PreProcess.Utils
  ( fieldOf
  , differKeys
  , existsObjectType
  , lookupType
  , getInputType
  , lookupField
  ) where

import           Data.List                           ((\\))
import           Data.Morpheus.Error.Variable        (unknownType)
import           Data.Morpheus.Schema.Internal.Types (InputType, InternalType (..), Leaf (..), OutputObject,
                                                      TypeLib (..))
import           Data.Morpheus.Types.Core            (EnhancedKey (..), Key, enhanceKeyWithNull)
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation, Validation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           (Text)

type GenError error a = error -> Either error a

lookupType :: error -> [(Text, a)] -> Text -> Either error a
lookupType error' lib' typeName' =
  case lookup typeName' lib' of
    Nothing -> Left error'
    Just x  -> pure x

lookupField :: [(Text, fType)] -> Text -> GenError error fType
lookupField outType fName error' =
  case lookup fName outType of
    Nothing    -> Left error'
    Just field -> pure field

getInputType :: Text -> TypeLib -> GenError error InputType
getInputType typeName' lib error' =
  case lookup typeName' (inputObject lib) of
    Just x -> pure (Object x)
    Nothing ->
      case lookup typeName' (leaf lib) of
        Nothing          -> Left error'
        Just (LScalar x) -> pure (Scalar x)
        Just (LEnum x y) -> pure (Enum x y)

existsObjectType :: Position -> Text -> TypeLib -> Validation OutputObject
existsObjectType position' typeName' lib = lookupType error' (object lib) typeName'
  where
    error' = unknownType typeName' position'

fieldOf :: (Position, Text) -> [(Text, fType)] -> Text -> MetaValidation fType
fieldOf (pos, tName) outType fName =
  case lookup fName outType of
    Nothing    -> Left $ UnknownField $ MetaInfo {key = fName, typeName = tName, position = pos}
    Just field -> pure field

differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys
