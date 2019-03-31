module Data.Morpheus.PreProcess.Utils
  ( fieldOf
  , differKeys
  , existsInputObjectType
  , existsObjectType
  , existsLeafType
  , toObject
  , getInputType
  -- , inputTypeBy
  -- , getObjectFieldType
  -- , getObjectFieldObjectType
  ) where

import           Data.List                           ((\\))
import           Data.Morpheus.Schema.Internal.Types (GObject (..), InputObject, InputType, InternalType (..),
                                                      Leaf (..), OutputObject, TypeLib (..))
import           Data.Morpheus.Types.Core            (EnhancedKey (..), Key, enhanceKeyWithNull)
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           as TX (Text)

--unwrapType :: GType -> Maybe GType
--unwrapType x =
--  case T.kind x of
--    EnumOf LIST -> T.ofType x
--    _           -> Just x
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

existsInputObjectType :: (Position, Key) -> TX.Text -> TypeLib -> MetaValidation InputObject
existsInputObjectType (position', key') typeName' lib = existsTypeIn (position', key') typeName' (inputObject lib)

existsLeafType :: (Position, Key) -> TX.Text -> TypeLib -> MetaValidation Leaf
existsLeafType (position', key') typeName' lib = existsTypeIn (position', key') typeName' (leaf lib)

--getFieldType :: Position -> TypeLib -> Field -> MetaValidation GType
-- getFieldType position' lib field = existsType (position', fieldName field) (fieldType field) lib
toObject :: MetaInfo -> InternalType a -> MetaValidation (GObject a)
toObject _ (Object gObj) = pure gObj
toObject meta (Scalar _) = Left $ UnknownType meta
toObject meta (Enum _ _) = Left $ UnknownType meta

--getInputFieldType :: Position -> TypeLib -> InputField -> MetaValidation InputType
--getInputFieldType position' lib (InputField field) = do
--  gType <- getFieldType position' lib field
--  case gType of
--    OType _ -> Left $ UnknownType $ MetaInfo {key = fieldName field, typeName = "", position = position'}
--    IType x -> pure x
-- inputTypeBy :: Position -> TypeLib -> [(Text, InputField)] -> Text -> MetaValidation InputType
-- inputTypeBy pos lib _parentType _name = fieldOf (pos, _name) _parentType _name >>= getInputFieldType pos lib
-- fType = ObjectField | InputField
fieldOf :: (Position, Text) -> [(Text, fType)] -> Text -> MetaValidation fType
fieldOf (pos, tName) outType fName =
  case lookup fName outType of
    Nothing    -> Left $ UnknownField $ MetaInfo {key = fName, typeName = tName, position = pos}
    Just field -> pure field

--getObjectFieldType :: Position -> TypeLib -> ObjectField -> MetaValidation OutputType
--getObjectFieldType position' lib field = do
--  gType <- getFieldType position' lib (fieldContent field)
--  case gType of
--    IType _ -> Left $ UnknownType $ MetaInfo {key = fieldName $ fieldContent field, typeName = "", position = position'}
--    OType x -> pure x
--getObjectFieldObjectType :: Position -> TypeLib -> ObjectField -> MetaValidation (GObject ObjectField)
-- getObjectFieldObjectType position' lib field = existsObjectType (position', key') typeName' lib position' lib (fieldContent field)
-- typeBy :: Position -> TypeLib -> [(Text, ObjectField)] -> Text -> MetaValidation OutputType
-- typeBy pos lib _parentType _name = fieldOf (pos, _name) _parentType _name >>= getObjectFieldType pos lib
differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys
