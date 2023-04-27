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
    TypeFingerprint,
    liftResult,
    toSchema,
  )
import Data.Morpheus.Server.Deriving.Utils.Types (CatType (OutputType), GQLTypeNode (..), fromSchema)
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

toDerivation :: TypeFingerprint -> GQLTypeNode c -> [NodeDerivation]
toDerivation fp (GQLTypeNode node xs) = TypeDerivation fp (toAny node) : map NodeExtension xs
toDerivation fp (GQLDirectiveNode node) = [DirectiveDerivation fp node]

resolveNode :: ScanProxy GQLType -> GQLResult [NodeDerivation]
resolveNode (ScanProxy proxy) = toDerivation (useFingerprint withGQL proxy) <$> useDeriveNode withGQL proxy

deriveSchema :: forall root f m e qu mu su. SCHEMA qu mu su => f (root m e qu mu su) -> GQLResult (Schema CONST)
deriveSchema _ =
  toSchema
    ( (,,,)
        <$> useDeriveRoot withGQL (Proxy @(qu IgnoredResolver))
        <*> traverse (useDeriveRoot withGQL) (ignoreUndefined (Proxy @(mu IgnoredResolver)))
        <*> traverse (useDeriveRoot withGQL) (ignoreUndefined (Proxy @(su IgnoredResolver)))
        <*> liftResult (fmap join (traverse resolveNode (explore (Proxy @qu) <> explore (Proxy @mu) <> explore (Proxy @su))))
    )
