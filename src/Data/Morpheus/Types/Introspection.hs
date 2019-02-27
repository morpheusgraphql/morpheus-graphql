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
  , GQL__EnumValue(..)
  , createInputValue
  , wrapListType
  , unwrapType
  , createScalar
  , createEnum
  , createInputObject
  )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           Data.Map                       ( Map
                                                , fromList
                                                )
import           GHC.Generics
import           Data.Aeson
import           Data.Data                      (Data)
import           Data.Morpheus.Types.Types      (EnumOf(..),(::->)(..))
import           Data.Morpheus.Schema.GQL__TypeKind
                                                ( GQL__TypeKind(..) )
import           Data.Morpheus.Schema.GQL__EnumValue
                                                ( GQL__EnumValue , createEnumValue)
import            Data.Maybe                     ( fromMaybe )
import qualified  Data.Morpheus.Schema.GQL__InputValue as I (GQL__InputValue(..),createInputValueWith)
import qualified  Data.Morpheus.Schema.GQL__Field as  F (GQL__Field(..), createFieldWith)
import            Data.Morpheus.Schema.GQL__Type  (GQL__Type(..), GQL__Deprecation__Args)

type GQL__InputValue = I.GQL__InputValue GQL__Type;
type GQL__Field =  F.GQL__Field GQL__Type;
type GQLTypeLib = Map Text GQL__Type

createInputValue :: Text -> Text -> I.GQL__InputValue GQL__Type
createInputValue name typeName = I.createInputValueWith name (createInputObject typeName [])

createField :: Text -> Text -> [I.GQL__InputValue GQL__Type] -> GQL__Field
createField name typeName args = F.createFieldWith name (createType typeName []) []


createInputObject  :: Text -> [GQL__Field]  -> GQL__Type
createInputObject name fields = GQL__Type {
  kind          = EnumOf INPUT_OBJECT
  , name          = name
  , description   = ""
  , fields        = Some fields
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some []
  , inputFields   = []
}

createType :: Text -> [GQL__Field] -> GQL__Type
createType name fields = GQL__Type
  { kind          = EnumOf OBJECT
  , name          = name
  , description   = ""
  , fields        = Some fields
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some []
  , inputFields   = []
  }

createScalar  :: Text -> GQL__Type
createScalar name  = GQL__Type {
  kind          = EnumOf SCALAR
  , name          = name
  , description   = ""
  , fields        = Some []
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some []
  , inputFields   = []
}

createEnum  :: Text -> [Text] -> GQL__Type
createEnum name tags = GQL__Type {
  kind          = EnumOf ENUM
  , name          = name
  , description   = ""
  , fields        = Some []
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some $ map createEnumValue tags
  , inputFields   = []
}


unwrapType :: GQL__Type -> Maybe GQL__Type
unwrapType x = case kind x of
  EnumOf LIST -> ofType x
  _    -> Just x

wrapListType :: GQL__Type -> GQL__Type
wrapListType contentType = GQL__Type
  { kind          = EnumOf LIST
  , name          = ""
  , description   = ""
  , fields        = None
  , ofType        = Just contentType
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = None
  , inputFields   = []
  }

emptyLib :: GQLTypeLib
emptyLib = fromList []