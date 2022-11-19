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
    SchemaT,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
  )
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Schema.Internal
  ( fromSchema,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Type
  ( useDeriveObject,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    ignoreUndefined,
    withGQL,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    toSchema,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    MUTATION,
    QUERY,
    SUBSCRIPTION,
    Schema (..),
  )
import Language.Haskell.TH (Exp, Q)
import Relude

type SCHEMA event (m :: Type -> Type) query mutation subscription =
  ( GQLType (query (Resolver QUERY event m)),
    GQLType (mutation (Resolver MUTATION event m)),
    GQLType (subscription (Resolver SUBSCRIPTION event m))
  )

-- | normal morpheus server validates schema at runtime (after the schema derivation).
--   this method allows you to validate it at compile time.
compileTimeSchemaValidation ::
  (SCHEMA event m qu mu su) =>
  proxy (root m event qu mu su) ->
  Q Exp
compileTimeSchemaValidation =
  fromSchema . (deriveSchema >=> validateSchema True defaultConfig)

deriveSchema :: forall root f m e qu mu su. SCHEMA e m qu mu su => f (root m e qu mu su) -> GQLResult (Schema CONST)
deriveSchema _ =
  toSchema
    ( (,,)
        <$> useDeriveObject withGQL (Proxy @(qu (Resolver QUERY e m)))
        <*> traverse (useDeriveObject withGQL) (ignoreUndefined (Proxy @(mu (Resolver MUTATION e m))))
        <*> traverse (useDeriveObject withGQL) (ignoreUndefined (Proxy @(su (Resolver SUBSCRIPTION e m))))
    )
