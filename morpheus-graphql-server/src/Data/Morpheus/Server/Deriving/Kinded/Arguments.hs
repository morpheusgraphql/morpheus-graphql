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
import Data.Morpheus.Internal.Utils (empty)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( runSchemaT,
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
import Relude hiding (empty)

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
    b <- fromMaybe empty . unFieldRep <$> useDeriveFieldArguments drvGQL (OutputType :: CatType OUT b)
    Just . fst <$> runSchemaT (a <:> b)
    where
      proxy = inputType (Proxy @a)