{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Domains.Users.Users where

import Data.Morpheus.Server.CodeGen.Internal
import Data.Morpheus.Server.Types
import Globals.GQLScalars

data User m = User
  { id :: m ID,
    name :: m Text
  }
  deriving (Generic)

instance (Typeable m) => GQLType (User m) where
  type KIND (User m) = TYPE
  directives _ =
    fieldDirective "id" Deprecated {reason = Just "XYZ"}
      <> fieldDirective "name" Deprecated {reason = Nothing}

data Query m = Query
  { users :: m [User m],
    user :: Arg "id" ID -> m (Maybe (User m))
  }
  deriving (Generic)

instance (Typeable m) => GQLType (Query m) where
  type KIND (Query m) = TYPE
