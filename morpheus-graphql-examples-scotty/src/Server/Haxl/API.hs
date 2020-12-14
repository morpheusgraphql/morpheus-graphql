{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Haxl.API
  ( app,
  )
where

import Data.Morpheus
  ( App,
    deriveApp,
  )
import Data.Morpheus.Types
  ( GQLType,
    ResolverQ,
    RootResolver (..),
    Undefined (..),
    liftEither,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Server.Haxl.Schema
  ( City,
    Deity,
    Human,
    dbDeity,
    someDeity,
    someHuman,
  )

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    deities :: m [Deity]
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text,
    bornPlace :: Maybe City
  }
  deriving (Generic, GQLType)

resolveDeity :: DeityArgs -> ResolverQ e IO Deity
resolveDeity DeityArgs {name, bornPlace} =
  liftEither $ dbDeity name bornPlace

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { deity = resolveDeity,
            deities = pure []
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

app :: App () IO
app = deriveApp rootResolver
