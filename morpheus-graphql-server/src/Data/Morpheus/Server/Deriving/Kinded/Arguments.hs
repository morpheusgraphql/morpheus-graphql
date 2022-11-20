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
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    withInput,
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

class DeriveFieldArguments gql a where
  deriveFieldArguments :: UseDeriving gql val -> f a -> SchemaT OUT (Maybe (ArgumentsDefinition CONST))

instance DeriveFieldArguments gql () where
  deriveFieldArguments _ _ = pure Nothing

instance (gql b, gql a) => DeriveFieldArguments gql (a -> b) where
  deriveFieldArguments UseDeriving {..} _ = do
    a <- withInput $ deriveTypeAsArguments dirGQL (Proxy @a)
    b <- useDeriveFieldArguments dirGQL (OutputType :: CatType OUT b)
    case b of
      Just x -> Just <$> (a <:> x)
      Nothing -> pure $ Just a
