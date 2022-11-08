{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Requests.TestFragments where

import Client.Schema
import Data.Morpheus.Client.CodeGen.Internal

instance RequestType TestFragments where
  type RequestArgs TestFragments = ()
  __name _ = "TestFragments"
  __query _ = "query TestFragments {\n  character {\n    ...DEITY\n    ...HERO\n  }\n  newTypes: character {\n    lastName\n    ...DEITY\n    ...HERO\n  }\n  person: character {\n    ...Person\n  }\n  heros: character {\n    ...HERO\n  }\n  superheros: character {\n    ...HERO\n  }\n}\n\nfragment Person on Character {\n  __typename\n  name\n  lastName\n}\n\nfragment DEITY on Deity {\n  __typename\n  name\n  power\n}\n\nfragment HERO on Hero {\n  __typename\n  name\n  hobby\n}"
  __type _ = OPERATION_QUERY

data TestFragments = TestFragments
  { character :: [TestFragmentsCharacter],
    newTypes :: [TestFragmentsNewTypes],
    person :: [FragmentPerson],
    heros :: [TestFragmentsHeros],
    superheros :: [TestFragmentsSuperheros]
  }
  deriving (Generic, Show, Eq)

instance FromJSON TestFragments where
  parseJSON =
    withObject "TestFragments" (\v -> TestFragments <$> v .: "character" <*> v .: "newTypes" <*> v .: "person" <*> v .: "heros" <*> v .: "superheros")

data TestFragmentsCharacter
  = TestFragmentsCharacterVariantDeity FragmentDEITY
  | TestFragmentsCharacterVariantHero FragmentHERO
  | TestFragmentsCharacter
  deriving (Generic, Show, Eq)

instance FromJSON TestFragmentsCharacter where
  parseJSON =
    withUnion
      ( \case
          ("Deity", v) -> TestFragmentsCharacterVariantDeity <$> parseJSON v
          ("Hero", v) -> TestFragmentsCharacterVariantHero <$> parseJSON v
          (_fallback, _) -> pure TestFragmentsCharacter
      )

data FragmentDEITY = FragmentDEITY
  { __typename :: String,
    name :: String,
    power :: Power
  }
  deriving (Generic, Show, Eq)

instance FromJSON FragmentDEITY where
  parseJSON =
    withObject "FragmentDEITY" (\v -> FragmentDEITY <$> v .: "__typename" <*> v .: "name" <*> v .: "power")

data FragmentHERO = FragmentHERO
  { __typename :: String,
    name :: String,
    hobby :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON FragmentHERO where
  parseJSON =
    withObject "FragmentHERO" (\v -> FragmentHERO <$> v .: "__typename" <*> v .: "name" <*> v .: "hobby")

data TestFragmentsNewTypes
  = TestFragmentsNewTypesVariantDeity TestFragmentsNewTypesDeity
  | TestFragmentsNewTypesVariantHero TestFragmentsNewTypesHero
  | TestFragmentsNewTypesVariantCharacter TestFragmentsNewTypesCharacter
  deriving (Generic, Show, Eq)

instance FromJSON TestFragmentsNewTypes where
  parseJSON =
    withUnion
      ( \case
          ("Deity", v) -> TestFragmentsNewTypesVariantDeity <$> parseJSON v
          ("Hero", v) -> TestFragmentsNewTypesVariantHero <$> parseJSON v
          (_fallback, v) -> TestFragmentsNewTypesVariantCharacter <$> parseJSON v
      )

data TestFragmentsNewTypesDeity = TestFragmentsNewTypesDeity
  { lastName :: Maybe String,
    __typename :: String,
    name :: String,
    power :: Power
  }
  deriving (Generic, Show, Eq)

instance FromJSON TestFragmentsNewTypesDeity where
  parseJSON =
    withObject "TestFragmentsNewTypesDeity" (\v -> TestFragmentsNewTypesDeity <$> v .:? "lastName" <*> v .: "__typename" <*> v .: "name" <*> v .: "power")

data TestFragmentsNewTypesHero = TestFragmentsNewTypesHero
  { lastName :: Maybe String,
    __typename :: String,
    name :: String,
    hobby :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON TestFragmentsNewTypesHero where
  parseJSON =
    withObject "TestFragmentsNewTypesHero" (\v -> TestFragmentsNewTypesHero <$> v .:? "lastName" <*> v .: "__typename" <*> v .: "name" <*> v .: "hobby")

newtype TestFragmentsNewTypesCharacter = TestFragmentsNewTypesCharacter
  { lastName :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON TestFragmentsNewTypesCharacter where
  parseJSON =
    withObject "TestFragmentsNewTypesCharacter" (\v -> TestFragmentsNewTypesCharacter <$> v .:? "lastName")

data FragmentPerson = FragmentPerson
  { __typename :: String,
    name :: String,
    lastName :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON FragmentPerson where
  parseJSON =
    withObject "FragmentPerson" (\v -> FragmentPerson <$> v .: "__typename" <*> v .: "name" <*> v .:? "lastName")

data TestFragmentsHeros
  = TestFragmentsHerosVariantHero FragmentHERO
  | TestFragmentsHeros
  deriving (Generic, Show, Eq)

instance FromJSON TestFragmentsHeros where
  parseJSON =
    withUnion
      ( \case
          ("Hero", v) -> TestFragmentsHerosVariantHero <$> parseJSON v
          (_fallback, _) -> pure TestFragmentsHeros
      )

data TestFragmentsSuperheros
  = TestFragmentsSuperherosVariantHero FragmentHERO
  | TestFragmentsSuperheros
  deriving (Generic, Show, Eq)

instance FromJSON TestFragmentsSuperheros where
  parseJSON =
    withUnion
      ( \case
          ("Hero", v) -> TestFragmentsSuperherosVariantHero <$> parseJSON v
          (_fallback, _) -> pure TestFragmentsSuperheros
      )
