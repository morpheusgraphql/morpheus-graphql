{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.App
  ( RootResolverConstraint,
    deriveSchema,
    deriveApp,
  )
where

-- MORPHEUS

import Data.Morpheus.App
  ( App (..),
    mkApp,
  )
import Data.Morpheus.App.Internal.Resolving
  ( resultOr,
  )
import Data.Morpheus.Server.Deriving.Encode
  ( EncodeConstraints,
    deriveModel,
  )
import Data.Morpheus.Server.Deriving.EncodeNamed
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

class DeriveApp f m (event :: *) (query :: (* -> *) -> *) (mut :: (* -> *) -> *) (sub :: (* -> *) -> *) where
  deriveApp :: f m event query mut sub -> App event m

instance RootResolverConstraint m e query mut sub => DeriveApp RootResolver m e query mut sub where
  deriveApp root =
    resultOr FailApp (uncurry mkApp) $
      (,) <$> deriveSchema (Identity root) <*> deriveModel root

instance NamedResolversConstraint m e query mut sub => DeriveApp NamedResolvers m e query mut sub where
  deriveApp root =
    resultOr FailApp (uncurry mkApp) $
      (,) <$> deriveSchema (Identity root) <*> deriveNamedModel root
