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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Named.Encode
  ( deriveNamedModel,
    EncodeNamedConstraints,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( NamedResolver (..),
    Resolver,
    RootResolverValue (..),
  )
import Data.Morpheus.NamedResolvers (NamedResolverT (..))
import Data.Morpheus.Server.Deriving.Named.EncodeType
  ( EncodeTypeConstraint,
    deriveResolver,
  )
import Data.Morpheus.Server.Deriving.Utils.GTraversable
  ( traverseTypes,
  )
import Data.Morpheus.Types
  ( NamedResolvers (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( QUERY,
  )
import qualified GHC.Exts as HM
import Relude

type EncodeNamedConstraints e m query mut sub =
  (EncodeTypeConstraint (Resolver QUERY e m) query)

deriveNamedModel ::
  forall e m query mut sub.
  (Monad m, EncodeNamedConstraints e m query mut sub) =>
  NamedResolvers m e query mut sub ->
  RootResolverValue e m
deriveNamedModel NamedResolvers =
  NamedResolversValue $
    HM.fromList $
      map (\x -> (resolverName x, x)) $
        join $
          toList $
            traverseTypes deriveResolver (Proxy @(query (NamedResolverT (Resolver QUERY e m))))
