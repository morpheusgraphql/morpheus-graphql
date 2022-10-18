{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.GetInterfaceTypes where

import Client.Schema
import Data.Morpheus.Client.CodeGen.Internal

instance RequestType GetInterfaceTypes where
  type RequestArgs GetInterfaceTypes = ()
  __name _ = "GetInterfaceTypes"
  __query _ = "query GetInterfaceTypes {\n  character {\n    name\n    ... on Deity {\n      power\n    }\n\n    ... on Hero {\n      hobby\n    }\n  }\n  character2: character {\n    name1: name\n    name\n  }\n  character3: character {\n    ... on Hero {\n      hobby\n    }\n    ... on Character {\n      name2: name\n    }\n  }\n  character4: character {\n    ... on Hero {\n      hobby\n    }\n  }\n}\n"
  __type _ = Query

data GetInterfaceTypes = GetInterfaceTypes
  { character :: [GetInterfaceTypesCharacterCharacter],
    character2 :: [GetInterfaceTypesCharacter2Character],
    character3 :: [GetInterfaceTypesCharacter3Character],
    character4 :: [GetInterfaceTypesCharacter4Character]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypes where
  parseJSON = withObject "GetInterfaceTypes" (\v -> GetInterfaceTypes <$> v .: "character" <*> v .: "character2" <*> v .: "character3" <*> v .: "character4")

data GetInterfaceTypesCharacterCharacter
  = GetInterfaceTypesCharacterDeity
      { name :: String,
        power :: Power
      }
  | GetInterfaceTypesCharacterHero
      { name :: String,
        hobby :: String
      }
  | GetInterfaceTypesCharacterCharacter
      { name :: String
      }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacterCharacter where
  parseJSON =
    takeValueType
      ( \case
          ("Deity", v) -> GetInterfaceTypesCharacterDeity <$> v .: "name" <*> v .: "power"
          ("Hero", v) -> GetInterfaceTypesCharacterHero <$> v .: "name" <*> v .: "hobby"
          (_fallback, v) -> GetInterfaceTypesCharacterCharacter <$> v .: "name"
      )

data GetInterfaceTypesCharacter2Character = GetInterfaceTypesCharacter2Character
  { name1 :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacter2Character where
  parseJSON = withObject "GetInterfaceTypesCharacter2Character" (\v -> GetInterfaceTypesCharacter2Character <$> v .: "name1" <*> v .: "name")

data GetInterfaceTypesCharacter3Character
  = GetInterfaceTypesCharacter3Hero
      { name2 :: String,
        hobby :: String
      }
  | GetInterfaceTypesCharacter3Character
      { name2 :: String
      }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacter3Character where
  parseJSON =
    takeValueType
      ( \case
          ("Hero", v) -> GetInterfaceTypesCharacter3Hero <$> v .: "name2" <*> v .: "hobby"
          (_fallback, v) -> GetInterfaceTypesCharacter3Character <$> v .: "name2"
      )

data GetInterfaceTypesCharacter4Character
  = GetInterfaceTypesCharacter4Hero
      { hobby :: String
      }
  | GetInterfaceTypesCharacter4Character
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacter4Character where
  parseJSON =
    takeValueType
      ( \case
          ("Hero", v) -> GetInterfaceTypesCharacter4Hero <$> v .: "hobby"
          (_fallback, _) -> pure GetInterfaceTypesCharacter4Character
      )
