{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Utils.Utils
  ( Type
  , Field
  , InputValue
  , TypeLib
  , createObjectType
  , createField
  , emptyLib
  , wrapListType
  ) where

import           Data.Map                        (Map, fromList)
import qualified Data.Morpheus.Schema.Field      as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..))
import           Data.Morpheus.Schema.Type       (Type (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind (..))
import           Data.Morpheus.Types.Describer   (EnumOf (..), WithDeprecationArgs (..))
import           Data.Text                       (Text)

type InputValue = I.InputValue Type

type Field = F.Field Type

type TypeLib = Map Text Type

createField :: Text -> Text -> [InputValue] -> Field
createField fName typeName = F.createFieldWith fName (createObjectType typeName "" [])

createObjectType :: Text -> Text -> [Field] -> Type
createObjectType tName desc tFields =
  Type
    { kind = EnumOf OBJECT
    , name = tName
    , description = desc
    , fields = WithDeprecationArgs tFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = []
    }

wrapListType :: Type -> Type
wrapListType contentType =
  Type
    { kind = EnumOf LIST
    , name = ""
    , description = ""
    , fields = WithDeprecationArgs []
    , ofType = Just contentType
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = []
    }

emptyLib :: TypeLib
emptyLib = fromList []
