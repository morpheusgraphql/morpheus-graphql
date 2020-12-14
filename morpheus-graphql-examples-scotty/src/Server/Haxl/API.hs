{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Server.Haxl.Schema
  ( Deity,
    Haxl,
    Realm,
    getAllDeityIds,
    getDeityById,
  )

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    deities :: m [Deity]
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text,
    bornPlace :: Maybe Realm
  }
  deriving (Generic, GQLType)

resolveDeity :: DeityArgs -> ResolverQ e Haxl Deity
resolveDeity DeityArgs {name} = getDeityById name

resolveDeities :: ResolverQ e Haxl [Deity]
resolveDeities = do
  userIds <- getAllDeityIds
  traverse getDeityById userIds

rootResolver :: RootResolver Haxl () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { deity = resolveDeity,
            deities = resolveDeities
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

app :: App () Haxl
app = deriveApp rootResolver

httpEndpoint :: App () Haxl -> App () IO
httpEndpoint = do
  -- Initialize Haxl state.
  let stateStore = stateSet UserState {} stateEmpty

  -- Initialize Haxl environment.
  env0 <- initEnv stateStore ()

  -- Run action.
  names <- runHaxl env0 getAllUsernames

  print names
