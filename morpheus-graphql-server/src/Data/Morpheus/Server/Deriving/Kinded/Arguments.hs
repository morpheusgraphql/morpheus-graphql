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

import Data.Morpheus.Internal.Ext ((<:>))
import Data.Morpheus.Server.Deriving.Internal.Schema.Internal
  ( CatType,
    deriveTypeAsArguments,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaT
  ( SchemaBuilder,
  )
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
  deriveFieldArguments :: UseDeriving gql val ~ ctx => ctx -> f a -> SchemaBuilder (Maybe (ArgumentsDefinition CONST))

instance DeriveFieldArguments ctx () where
  deriveFieldArguments _ _ = pure Nothing

instance (UseDeriving gql val ~ ctx, gql b, gql a) => DeriveFieldArguments ctx (a -> b) where
  deriveFieldArguments UseDeriving {..} _ = do
    a <- deriveTypeAsArguments drvGQL (Proxy @a)
    b <- unFieldRep <$> useDeriveFieldArguments drvGQL (OutputType :: CatType OUT b)
    case b of
      Just x -> Just <$> (a <:> x)
      Nothing -> pure $ Just a
