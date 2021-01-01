{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.Haxl.Schema
  ( Deity (..),
    Query (..),
    DeityArgs (..),
  )
where

import Data.Morpheus.Types
  ( GQLType (..),
    ID,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Deity m = Deity
  { name :: m Text,
    power :: m (Maybe Text)
  }
  deriving
    ( Generic,
      GQLType
    )

newtype DeityArgs = DeityArgs {deityId :: ID}
  deriving
    ( Generic,
      GQLType
    )

data Query m = Query
  { deity :: DeityArgs -> m (Deity m),
    deities :: m [Deity m]
  }
  deriving
    ( Generic,
      GQLType
    )
