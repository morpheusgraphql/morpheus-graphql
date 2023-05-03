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

module Data.Morpheus.Server.Deriving.Internal.Value
  ( DecodeRep (..),
    Context (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.App.Internal.Resolving (ResolverState)
import Data.Morpheus.Generic
  ( CProxy (..),
    DecodeFields,
    DecoderFun (..),
    DescribeCons,
    decodeFields,
    describeCons,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (inputType)
import Data.Morpheus.Server.Deriving.Utils.Types (coerceInputObject, getField)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseGQLValue (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
    ValidObject,
    ValidValue,
    Value (..),
    getInputUnionValue,
    internal,
    msg,
  )
import GHC.Generics
import Relude

data Context = Context
  { isVariantRef :: Bool,
    typeName :: TypeName,
    enumVisitor :: TypeName -> TypeName,
    fieldVisitor :: FieldName -> FieldName
  }

type DecoderT = ReaderT Context ResolverState

setVariantRef :: Bool -> DecoderT a -> DecoderT a
setVariantRef isVariantRef = local (\ctx -> ctx {isVariantRef})

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
decodeInputUnionObject ctx (l, r) name obj variant
  | [name] == l = L1 <$> decodeRep ctx variant
  | [name] == r = R1 <$> decodeRep ctx variant
  | otherwise = decideEither ctx (l, r) name (Object obj)

class DecodeRep ctx (f :: Type -> Type) where
  decodeRep :: ctx -> ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep ctx f) => DecodeRep ctx (M1 D d f) where
  decodeRep drv value = M1 <$> decodeRep drv value

instance (UseGQLType ctx gql, DescribeCons gql a, DescribeCons gql b, DecodeRep ctx a, DecodeRep ctx b) => DecodeRep ctx (a :+: b) where
  decodeRep ctx v =
    do
      typename <- asks typeName
      let (kind, (left, right)) = getUnionTags ctx typename (Proxy @(a :+: b))
      setVariantRef kind $
        case v of
          (Object obj) -> do
            (name, value) <- getInputUnionValue obj
            variant <- coerceInputObject value
            decodeInputUnionObject ctx (left, right) name obj (Object variant)
          (Enum name) -> do
            visitor <- asks enumVisitor
            decideEither ctx (map visitor left, map visitor right) name (Enum name)
          _ -> throwError (internal "lists and scalars are not allowed in Union")

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

getUnionTags ::
  forall ctx f a b gql.
  (UseGQLType ctx gql, DescribeCons gql a, DescribeCons gql b) =>
  ctx ->
  TypeName ->
  f (a :+: b) ->
  (Bool, ([TypeName], [TypeName]))
getUnionTags ctx typename _ = do
  let left = map toInfo (describeCons (Proxy @a))
  let right = map toInfo (describeCons (Proxy @b))
  let varRef = find snd (left <> right)
  (isJust varRef, (map fst left, map fst right))
  where
    toInfo :: (TypeName, Maybe (CProxy gql)) -> (TypeName, Bool)
    toInfo (consName, Just (CProxy p))
      | consName == typename <> typeVariant = (typeVariant, True)
      | otherwise = (consName, False)
      where
        typeVariant = useTypename ctx (inputType p)
    toInfo (consName, Nothing) = (consName, False)
