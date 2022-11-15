{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Kinded.Arguments
  ( DeriveArgs,
    DeriveFieldArguments (..),
    ForArgs,
    HasArguments,
  )
where

import Data.Morpheus.Internal.Ext ((<:>))
import Data.Morpheus.Internal.Utils (singleton)
import Data.Morpheus.Server.Deriving.Internal.Schema.Internal
  ( CatType,
    deriveTypeAsArguments,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.TypeContent
  ( injectType,
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveWith,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    ForAll,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy (symbolName)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
  )
import Data.Morpheus.Server.Types.Internal (TypeData (..))
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    withInput,
  )
import Data.Morpheus.Server.Types.Types
  ( Arg (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    IN,
    OUT,
    TypeRef (..),
    fieldsToArguments,
    mkField,
  )
import GHC.Generics
import GHC.TypeLits
import Relude

type family HasArguments a where
  HasArguments (a -> b) = (a -> b)
  HasArguments a = ()

unForAll :: f (ForAll a) -> Proxy a
unForAll _ = Proxy

toHasArgs :: f a -> Proxy (ForArgs a)
toHasArgs _ = Proxy

deriveArgs :: DeriveArgs gql (ForArgs a) => UseDeriving gql val -> f a -> SchemaT IN (ArgumentsDefinition CONST)
deriveArgs drv = __deriveArgs drv . toHasArgs

type family ForArgs a where
  ForArgs (Arg name a) = (Arg name a)
  ForArgs a = ForAll a

class DeriveFieldArguments gql a where
  deriveFieldArguments :: UseDeriving gql val -> f a -> SchemaT OUT (Maybe (ArgumentsDefinition CONST))

instance DeriveFieldArguments gql () where
  deriveFieldArguments _ _ = pure Nothing

instance (gql b, DeriveArgs gql (ForArgs a)) => DeriveFieldArguments gql (a -> b) where
  deriveFieldArguments drv@UseDeriving {..} _ = do
    a <- withInput $ deriveArgs drv (Proxy @a)
    b <- useDeriveFieldArguments dirGQL (OutputType :: CatType OUT b)
    case b of
      Just x -> Just <$> (a <:> x)
      Nothing -> pure $ Just a

class DeriveArgs gql a where
  __deriveArgs :: UseDeriving gql val -> f a -> SchemaT IN (ArgumentsDefinition CONST)

instance (gql a, DeriveWith gql gql (SchemaT IN (Maybe (ArgumentsDefinition CONST))) (Rep a)) => DeriveArgs gql (ForAll a) where
  __deriveArgs drv = deriveTypeAsArguments (dirGQL drv) . unForAll

instance (KnownSymbol name, gql a) => DeriveArgs gql (Arg name a) where
  __deriveArgs drv@UseDeriving {..} _ = do
    injectType drv proxy
    pure $ fieldsToArguments $ singleton argName $ mkField Nothing argName argTypeRef
    where
      proxy = InputType :: CatType IN a
      argName = symbolName (Proxy @name)
      argTypeRef = TypeRef {typeConName = gqlTypeName, typeWrappers = gqlWrappers}
      TypeData {gqlTypeName, gqlWrappers} = useTypeData dirGQL proxy
