{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Namespaces.Sophisticated where

import Data.Data (Typeable)
import Data.Morpheus ()
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Globals.GQLScalars

data TestEnum
  = TestEnumEnumA
  | TestEnumEnumB
  | TestEnumEnumC
  deriving (Generic, Show)

instance GQLType TestEnum where
  type KIND TestEnum = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "TestEnum"}
      <> enumDirective "TestEnumEnumA" Describe {text = "\n  enumValue Description: EnumA\n  "}
      <> enumDirective "TestEnumEnumB" Deprecated {reason = Just "test deprecation enumValue"}
      <> enumDirective "TestEnumEnumC" Deprecated {reason = Nothing}
      <> enumDirective "TestEnumEnumC" Describe {text = "\n  enumValue Description: EnumC\n  "}
      <> typeDirective Describe {text = "\ntype Description: TestEnum\n\nsome random enums for test\n"}

data CollidingEnum
  = CollidingEnumEnumA
  | CollidingEnumEnumB
  | CollidingEnumEnumC
  deriving (Generic, Show)

instance GQLType CollidingEnum where
  type KIND CollidingEnum = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "CollidingEnum"}
      <> enumDirective "CollidingEnumEnumA" Describe {text = "\n  enumValue Description: EnumA\n  "}
      <> enumDirective "CollidingEnumEnumB" Deprecated {reason = Just "test deprecation enumValue"}
      <> enumDirective "CollidingEnumEnumC" Deprecated {reason = Nothing}
      <> enumDirective "CollidingEnumEnumC" Describe {text = "\n  enumValue Description: EnumC\n  "}

newtype NestedInputObject = NestedInputObject
  { nestedInputObjectFieldTestID :: ID
  }
  deriving (Generic, Show)

instance GQLType NestedInputObject where
  type KIND NestedInputObject = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "NestedInputObject"}

data TestInputObject = TestInputObject
  { testInputObjectFieldTestScalar :: TestScalar,
    testInputObjectFieldNestedInputObject :: [Maybe NestedInputObject]
  }
  deriving (Generic, Show)

instance GQLType TestInputObject where
  type KIND TestInputObject = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "TestInputObject"}

data Coordinates = Coordinates
  { coordinatesLatitude :: TestScalar,
    coordinatesLongitude :: Int
  }
  deriving (Generic, Show)

instance GQLType Coordinates where
  type KIND Coordinates = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Coordinates"}
      <> fieldDirective "coordinatesLatitude" Describe {text = "\n  inputValue Description: latitude\n  "}
      <> fieldDirective "coordinatesLongitude" Describe {text = "\n  inputValue Description: longitude\n  some random inputValue details\n  "}
      <> typeDirective Describe {text = "\ntype Description: Coordinates\n\nsome random text\n"}

data Address m = Address
  { addressCity :: m Text,
    addressStreet :: AddressStreetArgs -> m (Maybe [Maybe [[[Text]]]]),
    addressHouseNumber :: m Int
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Address m) where
  type KIND (Address m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Address"}
      <> fieldDirective "addressCity" Deprecated {reason = Just "test deprecation field with reason"}
      <> fieldDirective "addressCity" Describe {text = "\n  field Description: city\n  "}
      <> fieldDirective "addressStreet" Deprecated {reason = Nothing}
      <> typeDirective Describe {text = "\ntype Description:\n\n  Address\n"}

data AddressStreetArgs = AddressStreetArgs
  { addressStreetArgsArgInputObject :: TestInputObject,
    addressStreetArgsArgMaybeString :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType AddressStreetArgs where
  type KIND AddressStreetArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "AddressStreetArgs"}
      <> fieldDirective "addressStreetArgsArgInputObject" Describe {text = "\n    argument Description: inputObject\n    "}

data TestUnion m
  = TestUnionUser
      { unTestUnionUser :: User m
      }
  | TestUnionAddress
      { unTestUnionAddress :: Address m
      }
  deriving (Generic)

instance (Typeable m) => GQLType (TestUnion m) where
  type KIND (TestUnion m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "TestUnion"}
      <> typeDirective Describe {text = "\ntype Description: TestUnion\n\nsome random text for union type\n"}

newtype InterfacePerson m = InterfacePerson
  { interfacePersonName :: m (Maybe Text)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (InterfacePerson m) where
  type KIND (InterfacePerson m) = TYPE
  directives _ =
    typeDirective Rename {newName = "Person"}
      <> typeDirective DropNamespace {dropNamespace = "InterfacePerson"}

type Person m = TypeGuard (InterfacePerson m) (User m)

data User m = User
  { userName :: m Text,
    userEmail :: m Text,
    userAddress :: UserAddressArgs -> m (Address m),
    userOffice :: UserOfficeArgs -> m (Address m),
    userFriend :: m (Maybe (User m))
  }
  deriving (Generic)

instance (Typeable m) => GQLType (User m) where
  type KIND (User m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "User"}
      <> fieldDirective "userName" Describe {text = "\n  field description: name\n  "}
      <> fieldDirective "userEmail" Describe {text = "\n  field description: email\n  "}
      <> typeDirective Describe {text = "\nUser\n\nMultilineDescription\n\nTest\n"}

data UserAddressArgs = UserAddressArgs
  { userAddressArgsCoordinates :: Coordinates,
    userAddressArgsComment :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType UserAddressArgs where
  type KIND UserAddressArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "UserAddressArgs"}

data UserOfficeArgs = UserOfficeArgs
  { userOfficeArgsZipCode :: Maybe [Int],
    userOfficeArgsCityID :: TestEnum
  }
  deriving (Generic, Show)

instance GQLType UserOfficeArgs where
  type KIND UserOfficeArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "UserOfficeArgs"}

data Query m = Query
  { queryUser :: m (User m),
    queryTestUnion :: m (Maybe (TestUnion m)),
    queryPerson :: m (Person m),
    queryTestEnum :: Arg "enum" CollidingEnum -> m [CollidingEnum]
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Query"}

newtype Mutation m = Mutation
  { mutationCreateUser :: MutationCreateUserArgs -> m (User m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Mutation m) where
  type KIND (Mutation m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Mutation"}

data MutationCreateUserArgs = MutationCreateUserArgs
  { mutationCreateUserArgsUserID :: Text,
    mutationCreateUserArgsUserName :: Maybe Text
  }
  deriving (Generic, Show)

instance GQLType MutationCreateUserArgs where
  type KIND MutationCreateUserArgs = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "MutationCreateUserArgs"}

data Subscription m = Subscription
  { subscriptionNewUser :: m (User m),
    subscriptionNewAddress :: m (Address m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Subscription m) where
  type KIND (Subscription m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "Subscription"}
