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

type TestUnderscoredType = Int

data TestCharCases
  = TestCharCasesLowerCaseA
  | TestCharCasesUpperCaseB
  | TestCharCases_underscoredC
  deriving (Generic, Show)

instance GQLType TestCharCases where
  type KIND TestCharCases = TYPE
  directives _ =
    typeDirective Rename {newName = "testCharCases"}
      <> typeDirective DropNamespace {dropNamespace = "testCharCases"}

data User m = User
  { userType :: m Text,
    userCharCases :: m (Maybe TestCharCases),
    user_underscored :: m (Maybe TestUnderscoredType)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (User m) where
  type KIND (User m) = TYPE
  directives _ = typeDirective DropNamespace {dropNamespace = "User"}

newtype MyQuery m = MyQuery
  { myQueryUser :: m (User m)
  }
  deriving (Generic)

instance (Typeable m) => GQLType (MyQuery m) where
  type KIND (MyQuery m) = TYPE
  directives _ = typeDirective DropNamespace {dropNamespace = "MyQuery"}
