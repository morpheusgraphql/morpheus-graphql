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
    SCHEMA,
  )
where

import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Internal.Schema.Internal
  ( CatType (OutputType),
    fromSchema,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Type
  ( useDeriveRoot,
  )
import Data.Morpheus.Server.Deriving.Utils.GScan
  ( ScanProxy (..),
    ScanRef,
    Scanner (..),
    scan,
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( NodeDerivation (..),
    SchemaBuilder,
    derivations,
    toSchema,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    IgnoredResolver,
    ignoreUndefined,
    withGQL,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    OUT,
    Schema (..),
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

exploreRef :: GQLType a => CatType c a -> [ScanRef GQLType]
exploreRef = useExploreRef withGQL

explore :: forall f (a :: (Type -> Type) -> Type). GQLType (a IgnoredResolver) => f a -> [ScanProxy GQLType]
explore _ = scan (Scanner exploreRef) (exploreRef (OutputType :: CatType OUT (a IgnoredResolver)))

deriveSchema :: forall root f m e qu mu su. SCHEMA qu mu su => f (root m e qu mu su) -> GQLResult (Schema CONST)
deriveSchema _ =
  toSchema
    ( (,,)
        <$> deriveQuery
        <*> traverse (useDeriveRoot withGQL) (ignoreUndefined (Proxy @(mu IgnoredResolver)))
        <*> traverse (useDeriveRoot withGQL) (ignoreUndefined (Proxy @(su IgnoredResolver)))
    )
  where
    deriveQuery = do
      let refs = explore (Proxy @qu) <> explore (Proxy @mu) <> explore (Proxy @su)
      traverse_ resolveRef refs
      useDeriveRoot withGQL (Proxy @(qu IgnoredResolver))

resolveRef :: ScanProxy GQLType -> SchemaBuilder ()
resolveRef (ScanProxy proxy) =
  useDeriveType withGQL proxy
    >>= \t -> derivations [TypeDerivation (useFingerprint withGQL proxy) (toAny t)]