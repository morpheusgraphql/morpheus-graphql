{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Requests.NewUsers where

import Client.Schema
import Data.Morpheus.Client.CodeGen.Internal

instance RequestType NewUsers where
  type RequestArgs NewUsers = NewUsersArgs
  __name _ = "NewUsers"
  __query _ = "subscription NewUsers($loc: Coordinates!) {\n  newUser {\n    name\n    email\n    address(coordinates: $loc) {\n      city\n    }\n    worships {\n      ... on Hero {\n        __typename\n        hobby\n      }\n    }\n  }\n}\n"
  __type _ = OPERATION_SUBSCRIPTION

newtype NewUsers = NewUsers
  { newUser :: NewUsersNewUser
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsers where
  parseJSON =
    withObject "NewUsers" (\v -> NewUsers <$> v .: "newUser")

data NewUsersNewUser = NewUsersNewUser
  { name :: String,
    email :: String,
    address :: NewUsersNewUserAddress,
    worships :: NewUsersNewUserWorships
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUser where
  parseJSON =
    withObject "NewUsersNewUser" (\v -> NewUsersNewUser <$> v .: "name" <*> v .: "email" <*> v .: "address" <*> v .: "worships")

newtype NewUsersNewUserAddress = NewUsersNewUserAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserAddress where
  parseJSON =
    withObject "NewUsersNewUserAddress" (\v -> NewUsersNewUserAddress <$> v .: "city")

data NewUsersNewUserWorships
  = NewUsersNewUserWorshipsVariantHero NewUsersNewUserWorshipsHero
  | NewUsersNewUserWorships
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserWorships where
  parseJSON =
    withUnion
      ( \case
          ("Hero", v) -> NewUsersNewUserWorshipsVariantHero <$> parseJSON v
          (_fallback, _) -> pure NewUsersNewUserWorships
      )

data NewUsersNewUserWorshipsHero = NewUsersNewUserWorshipsHero
  { __typename :: String,
    hobby :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserWorshipsHero where
  parseJSON =
    withObject "NewUsersNewUserWorshipsHero" (\v -> NewUsersNewUserWorshipsHero <$> v .: "__typename" <*> v .: "hobby")

newtype NewUsersArgs = NewUsersArgs
  { loc :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON NewUsersArgs where
  toJSON (NewUsersArgs newUsersArgsLoc) =
    omitNulls
      [ "loc" .= newUsersArgsLoc
      ]
