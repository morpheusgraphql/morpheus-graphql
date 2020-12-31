{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Haxl.API
  ( app,
    httpEndpoint,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Morpheus
  ( App,
    deriveApp,
    runApp,
  )
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Types
  ( ID,
    ResolverQ,
    RootResolver (..),
    Undefined (..),
    lift,
    render,
  )
import Data.Morpheus.Types.IO (MapAPI)
import Data.Text (Text)
import Haxl.Core (dataFetch, initEnv, runHaxl, stateEmpty, stateSet)
import Server.Haxl.DataSource
  ( DeityReq (..),
    Haxl,
    State (DeityState),
  )
import Server.Haxl.Schema
  ( Deity (..),
    DeityArgs (..),
    Query (..),
  )
import Server.Utils (isSchema)
import Web.Scotty
  ( RoutePattern,
    ScottyM,
    body,
    get,
    post,
    raw,
  )

getDeityIds :: ResolverQ e Haxl [ID]
getDeityIds = lift $ dataFetch GetAllIds

getDeityNameById :: ID -> ResolverQ e Haxl Text
getDeityNameById deiytId = lift $ dataFetch (GetNameById deiytId)

getDeityPowerById :: ID -> ResolverQ e Haxl (Maybe Text)
getDeityPowerById deiytId = lift $ dataFetch (GetPowerById deiytId)

getDeityById :: ID -> ResolverQ e Haxl Deity
getDeityById deityId = Deity <$> getDeityNameById deityId <*> getDeityPowerById deityId

resolveDeity :: DeityArgs -> ResolverQ e Haxl Deity
resolveDeity DeityArgs {deityId} = getDeityById deityId

resolveDeities :: ResolverQ e Haxl [Deity]
resolveDeities = do
  userIds <- getDeityIds
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

httpEndpoint ::
  RoutePattern ->
  App () Haxl ->
  ScottyM ()
httpEndpoint route app' = do
  get route $ (isSchema *> raw (render app)) <|> raw httpPlayground
  post route $ raw =<< (liftIO . runHaxlApp app' =<< body)

runHaxlApp :: MapAPI a b => App e Haxl -> a -> IO b
runHaxlApp haxlApp input = do
  let stateStore = stateSet DeityState stateEmpty
  enviroment <- initEnv stateStore ()
  runHaxl enviroment (runApp haxlApp input)
