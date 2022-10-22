{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Requests.GetInterfaceTypes where

import Client.Schema
import Data.Morpheus.Client.CodeGen.Internal
import Globals.GQLScalars

instance RequestType GetCharacters where
  type RequestArgs GetCharacters = ()
  __name _ = "GetCharacters"
  __query _ = "query GetCharacters {\n  character {\n    name\n    ...DEITY\n    ...HERO\n  }\n  anonymous: character {\n    name1: name\n    name\n  }\n  heros: character {\n    ...HERO\n    ... on Character {\n      name2: name\n    }\n  }\n  superheros: character {\n    ...HERO\n  }\n}\n\nfragment DEITY on Deity {\n  power\n}\n\nfragment HERO on Hero {\n  __typename\n  hobby\n}"
  __type _ = Query

data GetCharacters = GetCharacters
  { character :: [GetCharactersCharacter],
    anonymous :: [GetCharactersAnonymous],
    heros :: [GetCharactersHeros],
    superheros :: [GetCharactersSuperheros]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetCharacters where
  parseJSON =
    withObject "GetCharacters" (\v -> GetCharacters <$> v .: "character" <*> v .: "anonymous" <*> v .: "heros" <*> v .: "superheros")

data GetCharactersCharacter
  = GetCharactersCharacterVariantDeity GetCharactersCharacterDeity
  | GetCharactersCharacterVariantHero GetCharactersCharacterHero
  | GetCharactersCharacterVariantCharacter GetCharactersCharacterCharacter
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersCharacter where
  parseJSON =
    withUnion
      ( \case
          ("Deity", v) -> GetCharactersCharacterVariantDeity <$> parseJSON v
          ("Hero", v) -> GetCharactersCharacterVariantHero <$> parseJSON v
          (_fallback, v) -> GetCharactersCharacterVariantCharacter <$> parseJSON v
      )

data GetCharactersCharacterDeity = GetCharactersCharacterDeity
  { name :: String,
    power :: Power
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersCharacterDeity where
  parseJSON =
    withObject "GetCharactersCharacterDeity" (\v -> GetCharactersCharacterDeity <$> v .: "name" <*> v .: "power")

data GetCharactersCharacterHero = GetCharactersCharacterHero
  { name :: String,
    __typename :: String,
    hobby :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersCharacterHero where
  parseJSON =
    withObject "GetCharactersCharacterHero" (\v -> GetCharactersCharacterHero <$> v .: "name" <*> v .: "__typename" <*> v .: "hobby")

newtype GetCharactersCharacterCharacter = GetCharactersCharacterCharacter
  { name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersCharacterCharacter where
  parseJSON =
    withObject "GetCharactersCharacterCharacter" (\v -> GetCharactersCharacterCharacter <$> v .: "name")

data GetCharactersAnonymous = GetCharactersAnonymous
  { name1 :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersAnonymous where
  parseJSON =
    withObject "GetCharactersAnonymous" (\v -> GetCharactersAnonymous <$> v .: "name1" <*> v .: "name")

data GetCharactersHeros
  = GetCharactersHerosVariantHero GetCharactersHerosHero
  | GetCharactersHerosVariantCharacter GetCharactersHerosCharacter
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersHeros where
  parseJSON =
    withUnion
      ( \case
          ("Hero", v) -> GetCharactersHerosVariantHero <$> parseJSON v
          (_fallback, v) -> GetCharactersHerosVariantCharacter <$> parseJSON v
      )

data GetCharactersHerosHero = GetCharactersHerosHero
  { name2 :: String,
    __typename :: String,
    hobby :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersHerosHero where
  parseJSON =
    withObject "GetCharactersHerosHero" (\v -> GetCharactersHerosHero <$> v .: "name2" <*> v .: "__typename" <*> v .: "hobby")

newtype GetCharactersHerosCharacter = GetCharactersHerosCharacter
  { name2 :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersHerosCharacter where
  parseJSON =
    withObject "GetCharactersHerosCharacter" (\v -> GetCharactersHerosCharacter <$> v .: "name2")

data GetCharactersSuperheros
  = GetCharactersSuperherosVariantHero FragmentHERO
  | GetCharactersSuperheros
  deriving (Generic, Show, Eq)

instance FromJSON GetCharactersSuperheros where
  parseJSON =
    withUnion
      ( \case
          ("Hero", v) -> GetCharactersSuperherosVariantHero <$> parseJSON v
          (_fallback, _) -> pure GetCharactersSuperheros
      )

data FragmentHERO = FragmentHERO
  { __typename :: String,
    hobby :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON FragmentHERO where
  parseJSON =
    withObject "FragmentHERO" (\v -> FragmentHERO <$> v .: "__typename" <*> v .: "hobby")
