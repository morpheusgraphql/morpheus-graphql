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
  , createInputValue
  , wrapListType
  , createScalar
  , createEnum
  , createInputObject
  ) where

import           Data.Map                        (Map, fromList)
import           Data.Morpheus.Schema.EnumValue  (createEnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type       (Type (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind (..))
import           Data.Morpheus.Types.Describer   (EnumOf (..), WithDeprecationArgs (..))
import           Data.Text                       (Text)

type InputValue = I.InputValue Type

type Field = F.Field Type

type TypeLib = Map Text Type

createField :: Text -> Text -> [InputValue] -> Field
createField fName typeName = F.createFieldWith fName (createObjectType typeName "" [])

createInputValue :: Text -> Text -> InputValue
createInputValue iName typeName = I.createInputValueWith iName (createInputObject typeName [])

createInputObject :: Text -> [Field] -> Type
createInputObject iName iFields =
  Type
    { kind = EnumOf INPUT_OBJECT
    , name = iName
    , description = ""
    , fields = WithDeprecationArgs iFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = []
    }

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

createScalar :: Text -> Type
createScalar sName =
  Type
    { kind = EnumOf SCALAR
    , name = sName
    , description = ""
    , fields = WithDeprecationArgs []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = []
    }

createEnum :: Text -> [Text] -> Type
createEnum eName tags =
  Type
    { kind = EnumOf ENUM
    , name = eName
    , description = ""
    , fields = WithDeprecationArgs []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs $ map createEnumValue tags
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
