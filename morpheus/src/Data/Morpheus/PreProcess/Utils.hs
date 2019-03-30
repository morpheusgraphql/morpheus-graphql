{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Utils
  ( existsType
  , typeBy
  , fieldOf
  , fieldType
  , differKeys
  , inputTypeBy
  ) where

import           Data.List                           ((\\))
import qualified Data.Map                            as M (lookup)
import           Data.Morpheus.Schema.Internal.Types (Field (..), GType (..), InputField (..), InputType,
                                                      ObjectField (..), OutputType, TypeLib)
import           Data.Morpheus.Types.Core            (EnhancedKey (..), Key, enhanceKeyWithNull)
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           as TX (Text)

--unwrapType :: GType -> Maybe GType
--unwrapType x =
--  case T.kind x of
--    EnumOf LIST -> T.ofType x
--    _           -> Just x
existsType :: (Position, Key) -> TX.Text -> TypeLib -> MetaValidation GType
existsType (position', key') typeName' lib =
  case M.lookup typeName' lib of
    Nothing -> Left $ UnknownType meta
    Just x  -> pure x
  where
    meta = MetaInfo {position = position', typeName = typeName', key = key'}

getFieldType :: Position -> TypeLib -> Field -> MetaValidation GType
getFieldType position' lib field = existsType (position', fieldName field) (fieldType field) lib

getInputFieldType :: Position -> TypeLib -> InputField -> MetaValidation InputType
getInputFieldType position' lib (InputField field) = do
  gType <- getFieldType position' lib field
  case gType of
    OType _ -> Left $ UnknownType $ MetaInfo {key = fieldName field, typeName = "", position = position'}
    IType x -> pure x

inputTypeBy :: Position -> TypeLib -> [(Text, InputField)] -> Text -> MetaValidation InputType
inputTypeBy pos lib _parentType _name = fieldOf (pos, _name) _parentType _name >>= getInputFieldType pos lib

-- fType = ObjectField | InputField
fieldOf :: (Position, Text) -> [(Text, fType)] -> Text -> MetaValidation fType
fieldOf (pos, tName) outType fName =
  case lookup fName outType of
    Nothing    -> Left $ UnknownField $ MetaInfo {key = fName, typeName = tName, position = pos}
    Just field -> pure field

getObjectFieldType :: Position -> TypeLib -> ObjectField -> MetaValidation OutputType
getObjectFieldType position' lib field = do
  gType <- getFieldType position' lib (fieldContent field)
  case gType of
    IType _ -> Left $ UnknownType $ MetaInfo {key = fieldName $ fieldContent field, typeName = "", position = position'}
    OType x -> pure x

typeBy :: Position -> TypeLib -> [(Text, ObjectField)] -> Text -> MetaValidation OutputType
typeBy pos lib _parentType _name = fieldOf (pos, _name) _parentType _name >>= getObjectFieldType pos lib

differKeys :: [EnhancedKey] -> [Key] -> [EnhancedKey]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys
