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
  = GetInterfaceTypesCharacterCharacter
      { __typename :: String,
        name :: String
      }
  | GetInterfaceTypesCharacterDeity
      { __typename :: String,
        name :: String,
        power :: Power
      }
  | GetInterfaceTypesCharacterHero
      { __typename :: String,
        name :: String,
        hobby :: String
      }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacterCharacter where
  parseJSON =
    takeValueType
      ( \case
          ("Character", v) -> GetInterfaceTypesCharacterCharacter <$> v .: "__typename" <*> v .: "name"
          ("Deity", v) -> GetInterfaceTypesCharacterDeity <$> v .: "__typename" <*> v .: "name" <*> v .: "power"
          ("Hero", v) -> GetInterfaceTypesCharacterHero <$> v .: "__typename" <*> v .: "name" <*> v .: "hobby"
          (_, v) -> GetInterfaceTypesCharacterCharacter <$> v .: "__typename" <*> v .: "name"
      )

data GetInterfaceTypesCharacter2Character = GetInterfaceTypesCharacter2Character
  { __typename :: String,
    name1 :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacter2Character where
  parseJSON = withObject "GetInterfaceTypesCharacter2Character" (\v -> GetInterfaceTypesCharacter2Character <$> v .: "__typename" <*> v .: "name1" <*> v .: "name")

data GetInterfaceTypesCharacter3Character
  = GetInterfaceTypesCharacter3Character
      { __typename :: String,
        name2 :: String
      }
  | GetInterfaceTypesCharacter3Hero
      { __typename :: String,
        name2 :: String,
        hobby :: String
      }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacter3Character where
  parseJSON =
    takeValueType
      ( \case
          ("Character", v) -> GetInterfaceTypesCharacter3Character <$> v .: "__typename" <*> v .: "name2"
          ("Hero", v) -> GetInterfaceTypesCharacter3Hero <$> v .: "__typename" <*> v .: "name2" <*> v .: "hobby"
          (_, v) -> GetInterfaceTypesCharacter3Character <$> v .: "__typename" <*> v .: "name2"
      )

data GetInterfaceTypesCharacter4Character
  = GetInterfaceTypesCharacter4Character
      { __typename :: String
      }
  | GetInterfaceTypesCharacter4Hero
      { __typename :: String,
        hobby :: String
      }
  deriving (Generic, Show, Eq)

instance FromJSON GetInterfaceTypesCharacter4Character where
  parseJSON =
    takeValueType
      ( \case
          ("Character", v) -> GetInterfaceTypesCharacter4Character <$> v .: "__typename"
          ("Hero", v) -> GetInterfaceTypesCharacter4Hero <$> v .: "__typename" <*> v .: "hobby"
          (_, v) -> GetInterfaceTypesCharacter4Character <$> v .: "__typename"
      )
