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

data Deity = Deity
  { name :: Text,
    power :: Maybe Text
  }
  deriving
    ( Generic,
      Show,
      GQLType
    )

newtype DeityArgs = DeityArgs {deityId :: ID}
  deriving
    ( Generic,
      GQLType
    )

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    deities :: m [Deity]
  }
  deriving
    ( Generic,
      GQLType
    )
