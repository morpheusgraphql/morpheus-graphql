{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Feature.NamedResolvers.EntitiesApp
  ( entitiesApp,
  )
where

import Control.Monad.Except
import Data.Morpheus.Server (deriveApp)
import Data.Morpheus.Server.Resolvers
  ( NamedResolverT,
    NamedResolvers (..),
    ResolveNamed (..),
    ignoreBatching,
    resolve,
  )
import Data.Morpheus.Server.Types
  ( App,
    Arg (..),
    DecodeScalar (..),
    EncodeScalar (..),
    GQLError,
    GQLType (..),
    ID,
    SCALAR,
    Undefined,
  )
import Data.Text (Text)
import Feature.NamedResolvers.DB
  ( allDeities,
    allEntities,
  )
import Feature.NamedResolvers.RealmsApp (Deity, Realm)
import GHC.Generics (Generic)

newtype MyText = MyText Text
  deriving newtype
    ( DecodeScalar,
      EncodeScalar
    )

instance GQLType MyText where
  type KIND MyText = SCALAR

-- Entity
data Entity m
  = EntityDeity (m (Deity m))
  | EntityRealm (m (Realm m))
  deriving
    ( Generic,
      GQLType
    )

instance ResolveNamed m MyText where
  type Dep MyText = ID
  resolveBatched = pure . map (const $ Just $ MyText "x")

getEntity :: (MonadError GQLError m) => ID -> m (Entity (NamedResolverT m))
getEntity name | name `elem` allDeities = pure $ EntityDeity $ resolve $ pure name
getEntity x = pure $ EntityRealm $ resolve $ pure x

instance ResolveNamed m (Entity (NamedResolverT m)) where
  type Dep (Entity (NamedResolverT m)) = ID
  resolveBatched = ignoreBatching getEntity

-- QUERY
data Query m = Query
  { entities :: m [Entity m],
    entity :: Arg "id" ID -> m (Maybe (Entity m)),
    myText :: m MyText
  }
  deriving
    ( Generic,
      GQLType
    )

instance ResolveNamed m (Query (NamedResolverT m)) where
  type Dep (Query (NamedResolverT m)) = ()
  resolveBatched =
    ignoreBatching $
      const $
        pure
          Query
            { entities = resolve (pure allEntities),
              entity = \(Arg uid) -> resolve (pure uid),
              myText = resolve (pure ("" :: ID))
            }

entitiesApp :: App () IO
entitiesApp =
  deriveApp
    ( NamedResolvers ::
        NamedResolvers IO () Query Undefined Undefined
    )
