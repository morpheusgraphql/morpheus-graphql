{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Morpheus.Server.Deriving.Schema
  ( SchemaConstraints,
    deriveSchema,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Relude

type RootResolverConstraint m e query mutation subscription =
  ( EncodeConstraints e m query mutation subscription,
    SchemaConstraints e m query mutation subscription,
    Monad m
  )

deriveApp ::
  RootResolverConstraint m event query mut sub =>
  RootResolver m event query mut sub ->
  App event m
deriveApp root =
  resultOr FailApp (uncurry mkApp) $
    (,) <$> deriveSchema (Identity root) <*> deriveModel root
