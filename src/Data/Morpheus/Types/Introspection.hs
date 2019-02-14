{-# LANGUAGE OverloadedStrings , DeriveGeneric, DuplicateRecordFields , DeriveAnyClass , DeriveDataTypeable , TypeOperators  #-}

module Data.Morpheus.Types.Introspection
  ( GQL__Type(..)
  , GQL__Field(..)
  , GQL__TypeKind(..)
  , GQL__InputValue(..)
  , createType
  , createField
  , GQLTypeLib
  , emptyLib
  , GQL__EnumValue(..)
  , GQL__Deprication__Args(..)
  , createInputValue
  )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           Data.Map                       ( Map
                                                , fromList
                                                )
import           GHC.Generics
import           Data.Aeson                     ( ToJSON(..) )
import           Data.Data                      ( Data )
import           Data.GraphqlHS.Types.Types     ( (::->)(..) )
import           Data.GraphqlHS.Schema.GQL__TypeKind
                                                ( GQL__TypeKind(..) )
import           Data.GraphqlHS.Schema.GQL__EnumValue
                                                ( GQL__EnumValue )

data GQL__Type =  GQL__Type {
   kind :: GQL__TypeKind
  ,name :: Text
  ,description :: Text
  ,fields :: GQL__Deprication__Args ::-> [GQL__Field]
  ,ofType :: Maybe GQL__Type
  ,interfaces :: [GQL__Type]
  ,possibleTypes :: [GQL__Type]
  ,enumValues:: GQL__Deprication__Args ::-> [GQL__EnumValue]
  ,inputFields:: [GQL__InputValue]
} deriving (Show , Data, Generic, ToJSON )

data GQL__Deprication__Args = DepricationArgs {
  includeDeprecated:: Bool
} deriving (Show , Data, Generic, ToJSON )

data GQL__Field = GQL__Field{
  name:: Text,
  description:: Text,
  args:: [GQL__InputValue],
  _type :: Maybe GQL__Type,
  isDeprecated:: Bool,
  deprecationReason :: Text
} deriving (Show , Data, Generic, ToJSON )

data GQL__InputValue  = GQL__InputValue {
  name:: Text,
  description::  Text,
  _type:: Maybe GQL__Type,
  defaultValue::  Text
} deriving (Show , Data, Generic, ToJSON )

createInputValue :: Text -> Text -> GQL__InputValue
createInputValue argname typeName = GQL__InputValue
  { name         = argname
  , description  = ""
  , _type        = Just $ createType typeName []
  , defaultValue = ""
  }

type GQLTypeLib = Map Text GQL__Type;

createField :: Text -> Text -> [GQL__InputValue] -> GQL__Field
createField argname typeName args = GQL__Field
  { name              = argname
  , description       = "my description"
  , args              = args
  , _type             = Just $ createType typeName []
  , isDeprecated      = False
  , deprecationReason = ""
  }

createType :: Text -> [GQL__Field] -> GQL__Type
createType name fields = GQL__Type
  { kind          = OBJECT
  , name          = name
  , description   = "my description"
  , fields        = Some fields
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some []
  , inputFields   = []
  }

emptyLib :: GQLTypeLib
emptyLib = fromList []
