{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Decode.Rep
  ( DecodeRep (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.Generic
  ( DecodeFields,
    DecoderFun (..),
    decodeFields,
  )
import Data.Morpheus.Server.Deriving.Internal.Decode.Utils
  ( Context (..),
    DecoderT,
    DescribeCons,
    coerceInputObject,
    getField,
    getUnionInfos,
    setVariantRef,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType,
    UseGQLValue (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeName,
    ValidObject,
    ValidValue,
    Value (..),
    getInputUnionValue,
    internal,
    msg,
  )
import GHC.Generics
import Relude

decideEither ::
  (DecodeRep ctx f, DecodeRep ctx g) =>
  ctx ->
  ([TypeName], [TypeName]) ->
  TypeName ->
  ValidValue ->
  DecoderT ((f :+: g) a)
decideEither drv (left, right) name value
  | name `elem` left = L1 <$> decodeRep drv value
  | name `elem` right = R1 <$> decodeRep drv value
  | otherwise =
      throwError $
        internal $
          "Constructor \""
            <> msg name
            <> "\" could not find in Union"

decodeInputUnionObject ::
  (DecodeRep ctx f, DecodeRep ctx g) =>
  ctx ->
  ([TypeName], [TypeName]) ->
  TypeName ->
  ValidObject ->
  ValidValue ->
  DecoderT ((f :+: g) a)
decodeInputUnionObject drv (l, r) name unions variant
  | [name] == l = L1 <$> decodeRep drv variant
  | [name] == r = R1 <$> decodeRep drv variant
  | otherwise = decideEither drv (l, r) name (Object unions)

class DecodeRep ctx (f :: Type -> Type) where
  decodeRep :: ctx -> ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep ctx f) => DecodeRep ctx (M1 D d f) where
  decodeRep drv value = M1 <$> decodeRep drv value

instance (UseGQLType ctx gql, DescribeCons ctx a, DescribeCons ctx b, DecodeRep ctx a, DecodeRep ctx b) => DecodeRep ctx (a :+: b) where
  decodeRep ctx (Object obj) =
    do
      (kind, lr) <- getUnionInfos ctx (Proxy @(a :+: b))
      setVariantRef kind $ do
        (name, value) <- getInputUnionValue obj
        variant <- coerceInputObject value
        decodeInputUnionObject ctx lr name obj (Object variant)
  decodeRep ctx (Enum name) = do
    (_, (l, r)) <- getUnionInfos ctx (Proxy @(a :+: b))
    visitor <- asks enumVisitor
    decideEither ctx (map visitor l, map visitor r) name (Enum name)
  decodeRep _ _ = throwError (internal "lists and scalars are not allowed in Union")

instance (Constructor c, UseDeriving gql val ~ ctx, DecodeFields val a) => DecodeRep ctx (M1 C c a) where
  decodeRep ctx value = fmap M1 (decodeFields (decoder ctx value))

decoder :: (UseGQLValue ctx con) => ctx -> ValidValue -> DecoderFun con DecoderT
decoder ctx input =
  DecoderFun
    ( \name ->
        do
          Context {isVariantRef, fieldVisitor} <- ask
          value <- if isVariantRef then pure input else getField (fieldVisitor name) <$> coerceInputObject input
          lift (useDecodeValue ctx value)
    )
