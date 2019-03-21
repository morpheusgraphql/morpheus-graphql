{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

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
  , resolveDeprecation
  ) where

import           Data.Map                        (Map, fromList)
import           Data.Morpheus.Schema.EnumValue  (createEnumValue)
import qualified Data.Morpheus.Schema.Field      as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue as I (InputValue (..), createInputValueWith)
import           Data.Morpheus.Schema.Type       (Type (..))
import           Data.Morpheus.Schema.TypeKind   (TypeKind (..))
import           Data.Morpheus.Types.Describer   (Deprecation (..), EnumOf (..))
import           Data.Text                       (Text)

type InputValue = I.InputValue Type

type Field = F.Field Type

type TypeLib = Map Text Type

resolveDeprecation :: Deprecation b -> b
resolveDeprecation (Deprecation x) = x

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
    , fields = Deprecation iFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Deprecation []
    , inputFields = []
    }

createType :: Text -> [Field] -> Type
createType tName tFields =
  Type
    { kind = EnumOf OBJECT
    , name = tName
    , description = ""
    , fields = Deprecation tFields
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Deprecation []
    , inputFields = []
    }

createScalar :: Text -> Type
createScalar sName =
  Type
    { kind = EnumOf SCALAR
    , name = sName
    , description = ""
    , fields = Deprecation []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Deprecation []
    , inputFields = []
    }

createEnum :: Text -> [Text] -> Type
createEnum eName tags =
  Type
    { kind = EnumOf ENUM
    , name = eName
    , description = ""
    , fields = Deprecation []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = Deprecation $ map createEnumValue tags
    , inputFields = []
    }

wrapListType :: Type -> Type
wrapListType contentType =
  Type
    { kind = EnumOf LIST
    , name = ""
    , description = ""
    , fields = Deprecation []
    , ofType = Just contentType
    , interfaces = []
    , possibleTypes = []
    , enumValues = Deprecation []
    , inputFields = []
    }

emptyLib :: TypeLib
emptyLib = fromList []
