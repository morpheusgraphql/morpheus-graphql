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
import Data.Morpheus.Server.Deriving.Utils.Types (nodeToType, typeToArguments)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
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
  deriveFieldArguments :: UseDeriving gql val ~ ctx => ctx -> f a -> GQLResult (ArgumentsDefinition CONST)

instance DeriveFieldArguments ctx () where
  deriveFieldArguments _ _ = pure empty

instance (UseDeriving gql val ~ ctx, gql b, gql a) => DeriveFieldArguments ctx (a -> b) where
  deriveFieldArguments UseDeriving {..} _ = do
    a <- useDeriveNode drvGQL (inputType (Proxy @a)) >>= nodeToType >>= typeToArguments
    b <- useDeriveFieldArguments drvGQL (OutputType :: CatType OUT b)
    a <:> b
