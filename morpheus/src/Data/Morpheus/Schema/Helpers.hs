{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.Helpers
  ( Type
  , Field
  , InputValue
  , DeprecationArgs
  , EnumValue
  , TypeLib
  , createType
  , createField
  , emptyLib
  , createInputValue
  , wrapListType
  , createScalar
  , createEnum
  , createInputObject
  ) where

import           Data.Map                        (Map, fromList)
import           Data.Morpheus.Schema.EnumValue  (EnumValue, createEnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type       (DeprecationArgs, Type (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind (..))
import           Data.Morpheus.Types.Types       ((::->) (..), EnumOf (..))
import           Data.Text                       (Text)

type InputValue = I.InputValue Type

type Field = F.Field Type

type TypeLib = Map Text Type

createField :: Text -> Text -> [InputValue] -> Field
createField fName typeName = F.createFieldWith fName (createType typeName [])

createInputValue :: Text -> Text -> InputValue
createInputValue iName typeName = I.createInputValueWith iName (createInputObject typeName [])

createInputObject :: Text -> [Field] -> Type
createInputObject iName iFields =
  Type
    { kind = EnumOf INPUT_OBJECT
    , name = iName
    , description = ""
    , fields = Some iFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Some []
    , inputFields = []
    }

createType :: Text -> [Field] -> Type
createType tName tFields =
  Type
    { kind = EnumOf OBJECT
    , name = tName
    , description = ""
    , fields = Some tFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Some []
    , inputFields = []
    }

createScalar :: Text -> Type
createScalar sName =
  Type
    { kind = EnumOf SCALAR
    , name = sName
    , description = ""
    , fields = Some []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Some []
    , inputFields = []
    }

createEnum :: Text -> [Text] -> Type
createEnum eName tags =
  Type
    { kind = EnumOf ENUM
    , name = eName
    , description = ""
    , fields = Some []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Some $ map createEnumValue tags
    , inputFields = []
    }

wrapListType :: Type -> Type
wrapListType contentType =
  Type
    { kind = EnumOf LIST
    , name = ""
    , description = ""
    , fields = None
    , ofType = Just contentType
    , interfaces = []
    , possibleTypes = []
    , enumValues = None
    , inputFields = []
    }

emptyLib :: TypeLib
emptyLib = fromList []
