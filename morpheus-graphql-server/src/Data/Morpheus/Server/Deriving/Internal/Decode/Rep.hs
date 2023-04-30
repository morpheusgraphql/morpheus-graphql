{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Morpheus.Generic (CountFields (..))
import Data.Morpheus.Server.Deriving.Internal.Decode.Utils
  ( Context (..),
    DecoderT,
    DescribeCons,
    decodeFieldWith,
    getFieldName,
    getUnionInfos,
    setVariantRef,
    withInputObject,
    withInputUnion,
  )
import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( selNameProxy,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType,
    UseGQLValue (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Object,
    TypeName,
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
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
  Object VALID ->
  ValidObject ->
  DecoderT ((f :+: g) a)
decodeInputUnionObject drv (l, r) name unions object
  | [name] == l = L1 <$> decodeRep drv (Object object)
  | [name] == r = R1 <$> decodeRep drv (Object object)
  | otherwise = decideEither drv (l, r) name (Object unions)

class DecodeRep ctx (f :: Type -> Type) where
  decodeRep :: ctx -> ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep ctx f) => DecodeRep ctx (M1 D d f) where
  decodeRep drv value = M1 <$> decodeRep drv value

instance (UseGQLType ctx gql, DescribeCons ctx a, DescribeCons ctx b, DecodeRep ctx a, DecodeRep ctx b) => DecodeRep ctx (a :+: b) where
  decodeRep ctx (Object obj) =
    do
      (kind, lr) <- getUnionInfos ctx (Proxy @(a :+: b))
      setVariantRef kind $ withInputUnion (decodeInputUnionObject ctx lr) obj
  decodeRep ctx (Enum name) = do
    (_, (l, r)) <- getUnionInfos ctx (Proxy @(a :+: b))
    visitor <- asks enumVisitor
    decideEither ctx (map visitor l, map visitor r) name (Enum name)
  decodeRep _ _ = throwError (internal "lists and scalars are not allowed in Union")

instance (Constructor c, UseDeriving gql val ~ ctx, DecodeFields ctx a) => DecodeRep ctx (M1 C c a) where
  decodeRep ctx = fmap M1 . decodeFields ctx 0

class DecodeFields ctx (f :: Type -> Type) where
  decodeFields :: ctx -> Int -> ValidValue -> DecoderT (f a)

instance (DecodeFields val f, DecodeFields val g, CountFields g) => DecodeFields val (f :*: g) where
  decodeFields drv index gql =
    (:*:)
      <$> decodeFields drv index gql
      <*> decodeFields drv (index + countFields (Proxy @g)) gql

instance (Selector s, UseGQLValue ctx val, val a) => DecodeFields ctx (M1 S s (K1 i a)) where
  decodeFields val index value =
    M1 . K1 <$> do
      Context {isVariantRef, fieldVisitor} <- ask
      if isVariantRef
        then lift (useDecodeValue val value)
        else
          let fieldName = fieldVisitor $ getFieldName (selNameProxy (Proxy @s)) index
              fieldDecoder = decodeFieldWith (lift . useDecodeValue val) fieldName
           in withInputObject fieldDecoder value

instance DecodeFields val U1 where
  decodeFields _ _ _ = pure U1
