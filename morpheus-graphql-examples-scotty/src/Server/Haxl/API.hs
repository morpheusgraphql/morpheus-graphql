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
  ( ResolverQ,
    RootResolver (..),
    Undefined (..),
    render,
  )
import Data.Morpheus.Types.IO (MapAPI)
import Haxl.Core
import Server.Haxl.DataSource
  ( Haxl,
    State (DeityState),
    getAllDeityIds,
    getDeityById,
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
