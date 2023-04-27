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

module Data.Morpheus.Server.Deriving.Kinded.Arguments
  ( DeriveFieldArguments (..),
    HasArguments,
  )
where

import Data.Morpheus.Internal.Ext (GQLResult, (<:>))
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( unliftResult,
  )
import Data.Morpheus.Server.Deriving.Utils.Types (nodeToType, typeToArguments)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( FieldRep (..),
    UseDeriving (..),
    UseGQLType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    OUT,
  )
import Relude

type family HasArguments a where
  HasArguments (a -> b) = (a -> b)
  HasArguments a = ()

class DeriveFieldArguments ctx a where
  deriveFieldArguments :: UseDeriving gql val ~ ctx => ctx -> f a -> GQLResult (Maybe (ArgumentsDefinition CONST))

instance DeriveFieldArguments ctx () where
  deriveFieldArguments _ _ = pure Nothing

instance (UseDeriving gql val ~ ctx, gql b, gql a) => DeriveFieldArguments ctx (a -> b) where
  deriveFieldArguments UseDeriving {..} _ = do
    a <- useDeriveNode drvGQL proxy >>= nodeToType >>= typeToArguments
    b <- unFieldRep <$> useDeriveFieldArguments drvGQL (OutputType :: CatType OUT b)
    unliftResult $ case b of
      Just x -> Just <$> (a <:> x)
      Nothing -> pure $ Just a
    where
      proxy = inputType (Proxy @a)