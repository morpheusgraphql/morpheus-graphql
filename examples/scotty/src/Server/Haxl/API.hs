{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
    httpPlayground,
    runApp,
  )
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
import Server.Haxl.DataSource
  ( Haxl,
    getNamedIds,
    getNamedResponseById,
    withHaxl,
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

-- FETCH
getDeityIds :: Haxl [ID]
getDeityIds = getNamedIds

getDeityNameById :: ID -> Haxl Text
getDeityNameById = getNamedResponseById

getDeityPowerById :: ID -> Haxl (Maybe Text)
getDeityPowerById = getNamedResponseById

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

httpEndpoint ::
  RoutePattern ->
  App () Haxl ->
  ScottyM ()
httpEndpoint route app' = do
  get route $ (isSchema *> raw (render app)) <|> raw httpPlayground
  post route $ raw =<< (liftIO . withHaxl . runApp app' =<< body)