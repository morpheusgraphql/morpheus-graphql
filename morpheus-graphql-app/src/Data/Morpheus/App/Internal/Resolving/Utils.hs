{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Utils
  ( ResolverValue (..),
    requireObject,
    NamedResolverRef (..),
    ObjectTypeResolver,
    lookupResJSON,
    mkValue,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolverRef (..),
    ObjectTypeResolver (..),
    ResolverValue (..),
    mkList,
    mkNull,
    mkObjectMaybe,
  )
import Data.Morpheus.Internal.Utils (selectOr)
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    ScalarValue (..),
    TypeName,
    decodeScientific,
    internal,
    packName,
  )
import qualified Data.Vector as V
import Relude

lookupResJSON :: (MonadError GQLError f, Monad m) => Text -> A.Value -> f (ObjectTypeResolver m)
lookupResJSON name (A.Object fields) =
  selectOr
    mkEmptyObject
    (requireObject . mkValue)
    name
    fields
lookupResJSON _ _ = mkEmptyObject

mkEmptyObject :: Monad m => m (ObjectTypeResolver a)
mkEmptyObject = pure $ ObjectTypeResolver mempty

mkValue ::
  (Monad m) =>
  A.Value ->
  ResolverValue m
mkValue (A.Object v) =
  mkObjectMaybe
    (HM.lookup "__typename" v >>= unpackJSONName)
    $ fmap
      (bimap packName (pure . mkValue))
      (HM.toList v)
mkValue (A.Array ls) = mkList (fmap mkValue (V.toList ls))
mkValue A.Null = mkNull
mkValue (A.Number x) = ResScalar (decodeScientific x)
mkValue (A.String x) = ResScalar (String x)
mkValue (A.Bool x) = ResScalar (Boolean x)

requireObject :: MonadError GQLError f => ResolverValue m -> f (ObjectTypeResolver m)
requireObject (ResObject _ x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

unpackJSONName :: A.Value -> Maybe TypeName
unpackJSONName (A.String x) = Just (packName x)
unpackJSONName _ = Nothing
