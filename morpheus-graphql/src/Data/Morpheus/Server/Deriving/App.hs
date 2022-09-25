{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveSchema,
    deriveApp,
  )
where

import Data.Morpheus.App
  ( App (..),
    SchemaVisitors (..),
    mkApp,
  )
import Data.Morpheus.Internal.Ext (GQLResult, resultOr)
import Data.Morpheus.Server.Deriving.Encode
  ( EncodeConstraints,
    deriveModel,
  )
import Data.Morpheus.Server.Deriving.Named.Encode
  ( EncodeNamedConstraints,
    deriveNamedModel,
  )
import Data.Morpheus.Server.Deriving.Schema
  ( SchemaConstraints,
    deriveSchema,
  )
import Data.Morpheus.Types
  ( NamedResolvers,
    RootResolver (..),
  )
import Relude

type RootResolverConstraint m e query mutation subscription =
  ( EncodeConstraints e m query mutation subscription,
    SchemaConstraints e m query mutation subscription,
    Monad m
  )

type NamedResolversConstraint m e query mutation subscription =
  ( EncodeNamedConstraints e m query mutation subscription,
    SchemaConstraints e m query mutation subscription,
    Monad m
  )

class
  DeriveApp
    f
    m
    (event :: Type)
    (qu :: (Type -> Type) -> Type)
    (mu :: (Type -> Type) -> Type)
    (su :: (Type -> Type) -> Type)
  where
  deriveApp :: f m event qu mu su -> App event m

deriveVisitors :: p -> GQLResult SchemaVisitors
deriveVisitors _ =
  pure
    SchemaVisitors
      { typeVisitor = const pure,
        fieldVisitor = const pure
      }

instance RootResolverConstraint m e query mut sub => DeriveApp RootResolver m e query mut sub where
  deriveApp root =
    resultOr FailApp (\(x, y, z) -> mkApp x y z) $
      (,,) <$> deriveSchema (Identity root) <*> deriveModel root <*> deriveVisitors root

instance NamedResolversConstraint m e query mut sub => DeriveApp NamedResolvers m e query mut sub where
  deriveApp root =
    resultOr FailApp (\(x, y, z) -> mkApp x y z) $
      (,deriveNamedModel root,) <$> deriveSchema (Identity root) <*> deriveVisitors root
