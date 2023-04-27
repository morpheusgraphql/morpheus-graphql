{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
    UseValue (..),
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
  (DecodeRep gql args f, DecodeRep gql args g) =>
  UseDeriving gql args ->
  ([TypeName], [TypeName]) ->
  TypeName ->
  ValidValue ->
  DecoderT ((f :+: g) a)
decideEither dir (left, right) name value
  | name `elem` left = L1 <$> decodeRep dir value
  | name `elem` right = R1 <$> decodeRep dir value
  | otherwise =
      throwError $
        internal $
          "Constructor \""
            <> msg name
            <> "\" could not find in Union"

decodeInputUnionObject ::
  (DecodeRep gql args f, DecodeRep gql args g) =>
  UseDeriving gql args ->
  ([TypeName], [TypeName]) ->
  TypeName ->
  Object VALID ->
  ValidObject ->
  DecoderT ((f :+: g) a)
decodeInputUnionObject dir (l, r) name unions object
  | [name] == l = L1 <$> decodeRep dir (Object object)
  | [name] == r = R1 <$> decodeRep dir (Object object)
  | otherwise = decideEither dir (l, r) name (Object unions)

class DecodeRep gql args (f :: Type -> Type) where
  decodeRep :: UseDeriving gql args -> ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep gql args f) => DecodeRep gql args (M1 D d f) where
  decodeRep dir value = M1 <$> decodeRep dir value

instance (DescribeCons gql a, DescribeCons gql b, DecodeRep gql args a, DecodeRep gql args b) => DecodeRep gql args (a :+: b) where
  decodeRep dir (Object obj) =
    do
      (kind, lr) <- getUnionInfos (drvGQL dir) (Proxy @(a :+: b))
      setVariantRef kind $ withInputUnion (decodeInputUnionObject dir lr) obj
  decodeRep dir (Enum name) = do
    (_, (l, r)) <- getUnionInfos (drvGQL dir) (Proxy @(a :+: b))
    visitor <- asks enumVisitor
    decideEither dir (map visitor l, map visitor r) name (Enum name)
  decodeRep _ _ = throwError (internal "lists and scalars are not allowed in Union")

instance (Constructor c, DecodeFields gql args a) => DecodeRep gql args (M1 C c a) where
  decodeRep dir = fmap M1 . decodeFields dir 0

class DecodeFields gql args (f :: Type -> Type) where
  decodeFields :: UseDeriving gql args -> Int -> ValidValue -> DecoderT (f a)

instance (DecodeFields gql args f, DecodeFields gql args g, CountFields g) => DecodeFields gql args (f :*: g) where
  decodeFields dir index gql =
    (:*:)
      <$> decodeFields dir index gql
      <*> decodeFields dir (index + countFields (Proxy @g)) gql

instance (Selector s, args a) => DecodeFields gql args (M1 S s (K1 i a)) where
  decodeFields UseDeriving {drvArgs} index value =
    M1 . K1 <$> do
      Context {isVariantRef, fieldVisitor} <- ask
      if isVariantRef
        then lift (useDecodeValue drvArgs value)
        else
          let fieldName = fieldVisitor $ getFieldName (selNameProxy (Proxy @s)) index
              fieldDecoder = decodeFieldWith (lift . useDecodeValue drvArgs) fieldName
           in withInputObject fieldDecoder value

instance DecodeFields gql args U1 where
  decodeFields _ _ _ = pure U1
