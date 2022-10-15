{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Namespaces.Cases where

import Data.Data (Typeable)
import Data.Morpheus ()
import Data.Morpheus.Kind (TYPE)
import Data.Morpheus.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Globals.GQLScalars

data TestUnderscoredType = TestUnderscoredType_TestUnderscoredType
  deriving (Generic, Show)

instance GQLType TestUnderscoredType where
  type KIND TestUnderscoredType = TYPE
  directives _ =
    typeDirective Rename {newName = "_TestUnderscoredType"}
      <> typeDirective DropNamespace {dropNamespace = "TestUnderscoredType"}
      <> enumDirective "TestUnderscoredType_TestUnderscoredType" Rename {newName = "_TestUnderscoredType"}

data TestCharCases
  = TestCharCasesLowerCaseA
  | TestCharCasesUpperCaseB
  | TestCharCases_underscoredC
  deriving (Generic, Show)

instance GQLType TestCharCases where
  type KIND TestCharCases = TYPE
  directives _ =
    typeDirective Rename {newName = "testCharCases"}
      <> typeDirective DropNamespace {dropNamespace = "TestCharCases"}
      <> enumDirective "TestCharCasesLowerCaseA" Rename {newName = "lowerCaseA"}
      <> enumDirective "TestCharCases_underscoredC" Rename {newName = "_underscoredC"}

data User m = User
  { userType :: m Text,
    userCharCases :: m (Maybe TestCharCases),
    user_underscored :: m (Maybe TestUnderscoredType)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (User m) where
  type KIND (User m) = TYPE
  directives _ =
    typeDirective DropNamespace {dropNamespace = "User"}
      <> fieldDirective "userCharCases" Rename {newName = "CharCases"}

newtype MyQuery m = MyQuery
  { myQueryUser :: m (User m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (MyQuery m) where
  type KIND (MyQuery m) = TYPE
  directives _ = typeDirective DropNamespace {dropNamespace = "MyQuery"}
