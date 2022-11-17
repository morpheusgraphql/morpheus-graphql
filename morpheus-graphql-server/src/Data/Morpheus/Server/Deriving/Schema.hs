{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
    deriveSchema,
    SchemaConstraints,
    SchemaT,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
  )
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Internal.Ext
import Data.Morpheus.Server.Deriving.Internal.Schema.Internal
  ( fromSchema,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Object
  ( asObjectType,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.TypeContent
import Data.Morpheus.Server.Deriving.Kinded.Type
  ( DERIVE_TYPE,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (outputType)
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    ignoreUndefined,
    withDir,
    withGQL,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    toSchema,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    MUTATION,
    OBJECT,
    OUT,
    QUERY,
    SUBSCRIPTION,
    Schema (..),
    TypeDefinition (..),
  )
import Language.Haskell.TH (Exp, Q)
import Relude

type SchemaConstraints event (m :: Type -> Type) query mutation subscription =
  ( DERIVE_TYPE GQLType OUT (query (Resolver QUERY event m)),
    DERIVE_TYPE GQLType OUT (mutation (Resolver MUTATION event m)),
    DERIVE_TYPE GQLType OUT (subscription (Resolver SUBSCRIPTION event m))
  )

-- | normal morpheus server validates schema at runtime (after the schema derivation).
--   this method allows you to validate it at compile time.
compileTimeSchemaValidation ::
  (SchemaConstraints event m qu mu su) =>
  proxy (root m event qu mu su) ->
  Q Exp
compileTimeSchemaValidation =
  fromSchema . (deriveSchema >=> validateSchema True defaultConfig)

deriveSchema ::
  forall
    root
    proxy
    m
    e
    query
    mut
    subs.
  ( SchemaConstraints e m query mut subs
  ) =>
  proxy (root m e query mut subs) ->
  GQLResult (Schema CONST)
deriveSchema _ =
  toSchema
    ( (,,)
        <$> deriveRoot (Proxy @(query (Resolver QUERY e m)))
        <*> traverse deriveRoot (ignoreUndefined (Proxy @(mut (Resolver MUTATION e m))))
        <*> traverse deriveRoot (ignoreUndefined (Proxy @(subs (Resolver SUBSCRIPTION e m))))
    )

deriveRoot :: DERIVE_TYPE GQLType OUT a => f a -> SchemaT OUT (TypeDefinition OBJECT CONST)
deriveRoot = asObjectType withGQL (deriveFields withDir . outputType)
