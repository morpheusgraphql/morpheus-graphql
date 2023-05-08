{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
    deriveSchema,
    SCHEMA,
  )
where

import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (toAssoc)
import Data.Morpheus.Server.Deriving.Utils.GScan
  ( ScanProxy (..),
    Scanner (..),
    scan,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (outputType)
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( NodeDerivation (..),
    SchemaState (..),
    TypeFingerprint,
    checkTypeCollisions,
    execNode,
    insertImplements,
  )
import Data.Morpheus.Server.Deriving.Utils.Types (CatType (OutputType), GQLTypeNode (..), coerceObject, fromSchema, nodeToType)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseGQLType (useDeriveNode, useExploreRef, useFingerprint),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    IgnoredResolver,
    ignoreUndefined,
    withGQL,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    OBJECT,
    OUT,
    Schema (..),
    TypeDefinition,
    defineDirective,
    defineSchemaWith,
    toAny,
  )
import Language.Haskell.TH (Exp, Q)
import Relude

type SCHEMA qu mu su =
  ( GQLType (qu IgnoredResolver),
    GQLType (mu IgnoredResolver),
    GQLType (su IgnoredResolver)
  )

-- | normal morpheus server validates schema at runtime (after the schema derivation).
--   this method allows you to validate it at compile time.
compileTimeSchemaValidation :: (SCHEMA qu mu su) => proxy (root m event qu mu su) -> Q Exp
compileTimeSchemaValidation = fromSchema . (deriveSchema >=> validateSchema True defaultConfig)

explore :: forall f (a :: (Type -> Type) -> Type). (GQLType (a IgnoredResolver)) => f a -> [ScanProxy GQLType]
explore _ = scan (Scanner (useExploreRef withGQL)) (OutputType :: CatType OUT (a IgnoredResolver))

toDerivation :: TypeFingerprint -> GQLTypeNode c -> [NodeDerivation]
toDerivation fp (GQLTypeNode node xs) = TypeDerivation fp (toAny node) : map NodeExtension xs
toDerivation fp (GQLDirectiveNode node) = [DirectiveDerivation fp node]

resolveNode :: ScanProxy GQLType -> GQLResult [NodeDerivation]
resolveNode (ScanProxy proxy) = toDerivation (useFingerprint withGQL proxy) <$> useDeriveNode withGQL proxy

deriveRoot :: (GQLType a) => f a -> GQLResult (TypeDefinition OBJECT CONST)
deriveRoot prx = useDeriveNode withGQL (outputType prx) >>= nodeToType >>= coerceObject

deriveSchema :: forall root f m e qu mu su. (SCHEMA qu mu su) => f (root m e qu mu su) -> GQLResult (Schema CONST)
deriveSchema _ = do
  query <- deriveRoot (Proxy @(qu IgnoredResolver))
  mutation <- traverse deriveRoot (ignoreUndefined (Proxy @(mu IgnoredResolver)))
  subscription <- traverse deriveRoot (ignoreUndefined (Proxy @(su IgnoredResolver)))
  typeNodes <- fmap join (traverse resolveNode (explore (Proxy @qu) <> explore (Proxy @mu) <> explore (Proxy @su)))
  SchemaState {..} <- foldlM (&) mempty (map execNode typeNodes)
  types <- map (insertImplements implements) <$> checkTypeCollisions (toAssoc typeDefinitions)
  schema <- defineSchemaWith types (Just query, mutation, subscription)
  foldlM defineDirective schema directiveDefinitions
