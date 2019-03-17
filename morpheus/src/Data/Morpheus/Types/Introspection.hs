{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Types.Introspection
  ( GQL__Type
  , GQL__Field
  , GQL__InputValue
  , GQL__Deprecation__Args
  , GQL__TypeKind(..)
  , createType
  , createField
  , GQLTypeLib
  , emptyLib
  , GQL__EnumValue
  , createInputValue
  , wrapListType
  , createScalar
  , createEnum
  , createInputObject
  ) where

import           Data.Map                             (Map, fromList)
import Data.Morpheus.Schema.GQL__EnumValue (GQL__EnumValue, createEnumValue)import qualified Data.Morpheus.Schema.GQL__Field      as F (GQL__Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.GQL__InputValue as I (GQL__InputValue (..),
                                                            createInputValueWith)
import           Data.Morpheus.Schema.GQL__Type       (GQL__Deprecation__Args, GQL__Type (..))
import           Data.Morpheus.Schema.GQL__TypeKind   (GQL__TypeKind (..))
import           Data.Morpheus.Types.Types            ((::->) (..), EnumOf (..))
import           Data.Text                            (Text)

type GQL__InputValue = I.GQL__InputValue GQL__Type

type GQL__Field = F.GQL__Field GQL__Type

type GQLTypeLib = Map Text GQL__Type

createField :: Text -> Text -> [I.GQL__InputValue GQL__Type] -> GQL__Field
createField fName typeName = F.createFieldWith fName (createType typeName [])

createInputValue :: Text -> Text -> I.GQL__InputValue GQL__Type
createInputValue iName typeName = I.createInputValueWith iName (createInputObject typeName [])

createInputObject :: Text -> [GQL__Field] -> GQL__Type
createInputObject iName iFields =
  GQL__Type
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

createType :: Text -> [GQL__Field] -> GQL__Type
createType tName tFields =
  GQL__Type
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

createScalar :: Text -> GQL__Type
createScalar sName =
  GQL__Type
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

createEnum :: Text -> [Text] -> GQL__Type
createEnum eName tags =
  GQL__Type
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

wrapListType :: GQL__Type -> GQL__Type
wrapListType contentType =
  GQL__Type
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

emptyLib :: GQLTypeLib
emptyLib = fromList []
