{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HaxlAPI.API
  ( httpEndpoint,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Morpheus
  ( App,
    deriveApp,
    httpPlayground,
    runApp,
  )
import Data.Morpheus.App (MapAPI)
import Data.Morpheus.Types
  ( ComposedResolver,
    ID,
    QUERY,
    Resolver,
    ResolverQ,
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    lift,
    render,
  )
import Data.Text (Text)
import Haxl.Core (dataFetch, initEnv, runHaxl, stateEmpty, stateSet)
import HaxlAPI.DataSource
  ( DeityReq (..),
    Haxl,
    State (DeityState),
  )
import HaxlAPI.Schema
  ( Deity (..),
    DeityArgs (..),
    Query (..),
  )
import Web.Scotty
  ( ActionM,
    RoutePattern,
    ScottyM,
    body,
    get,
    param,
    post,
    raw,
  )

-- FETCH
getDeityIds :: Haxl [ID]
getDeityIds = dataFetch GetDeityIds

getDeityNameById :: ID -> Haxl Text
getDeityNameById = dataFetch . GetDeityNameById

getDeityPowerById :: ID -> Haxl (Maybe Text)
getDeityPowerById = dataFetch . GetDeityPowerById

-- RESOLVERS
getDeityById :: ID -> ResolverQ e Haxl Deity
getDeityById deityId =
  pure
    Deity
      { name = lift (getDeityNameById deityId),
        power = lift (getDeityPowerById deityId)
      }

resolveDeity :: DeityArgs -> ResolverQ e Haxl Deity
resolveDeity DeityArgs {deityId} = getDeityById deityId

resolveDeities :: ComposedResolver QUERY e Haxl [] Deity
resolveDeities = do
  ids <- lift getDeityIds
  traverse getDeityById ids

resolveQuery :: Query (Resolver QUERY e Haxl)
resolveQuery =
  Query
    { deity = resolveDeity,
      deities = resolveDeities
    }

rootResolver :: RootResolver Haxl () Query Undefined Undefined
rootResolver = defaultRootResolver {queryResolver = resolveQuery}

app :: App () Haxl
app = deriveApp rootResolver

isSchema :: ActionM String
isSchema = param "schema"

httpEndpoint :: RoutePattern -> ScottyM ()
httpEndpoint route = do
  get route $ (isSchema *> raw (render app)) <|> raw httpPlayground
  post route $ raw =<< (liftIO . runHaxlApp (deriveApp rootResolver) =<< body)

runHaxlApp :: (MapAPI a b) => App e Haxl -> a -> IO b
runHaxlApp haxlApp input = do
  let stateStore = stateSet DeityState stateEmpty
  environment <- initEnv stateStore ()
  runHaxl environment (runApp haxlApp input)
