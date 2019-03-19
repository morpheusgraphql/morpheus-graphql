{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.Utils.Utils
  ( Type
  , Field
  , InputValue
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
import           Data.Morpheus.Schema.EnumValue  (createEnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type       (Type (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind (..))
import           Data.Morpheus.Types.Describer   ((::->) (..), EnumOf (..))
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
    , fields = Resolved iFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Resolved []
    , inputFields = []
    }

createType :: Text -> [Field] -> Type
createType tName tFields =
  Type
    { kind = EnumOf OBJECT
    , name = tName
    , description = ""
    , fields = Resolved tFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Resolved []
    , inputFields = []
    }

createScalar :: Text -> Type
createScalar sName =
  Type
    { kind = EnumOf SCALAR
    , name = sName
    , description = ""
    , fields = Resolved []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Resolved []
    , inputFields = []
    }

createEnum :: Text -> [Text] -> Type
createEnum eName tags =
  Type
    { kind = EnumOf ENUM
    , name = eName
    , description = ""
    , fields = Resolved []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Resolved $ map createEnumValue tags
    , inputFields = []
    }

wrapListType :: Type -> Type
wrapListType contentType =
  Type
    { kind = EnumOf LIST
    , name = ""
    , description = ""
    , fields = Resolved []
    , ofType = Just contentType
    , interfaces = []
    , possibleTypes = []
    , enumValues = Resolved []
    , inputFields = []
    }

emptyLib :: TypeLib
emptyLib = fromList []
