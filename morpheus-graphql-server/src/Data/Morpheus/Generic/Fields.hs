{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Generic.Fields
  ( DecoderFun (..),
    DecodeFields,
    decodeFields,
    CountFields (..),
  )
where

import Data.Morpheus.Server.Deriving.Utils.Proxy
  ( selNameProxy,
  )
import Data.Morpheus.Types.Internal.AST (FieldName)
import GHC.Generics
import Relude

class CountFields (f :: Type -> Type) where
  countFields :: Proxy f -> Int

instance (CountFields f, CountFields g) => CountFields (f :*: g) where
  countFields _ = countFields (Proxy @f) + countFields (Proxy @g)

instance (Selector s) => CountFields (M1 S s (K1 i a)) where
  countFields _ = 1

instance CountFields U1 where
  countFields _ = 0

newtype DecoderFun con m = DecoderFun {decoderFun :: forall a. (con a) => FieldName -> m a}

decodeFields :: (Monad m, DecodeFields con f) => DecoderFun con m -> m (f a)
decodeFields ctx = decodeFieldsWith ctx 0

class DecodeFields con (f :: Type -> Type) where
  decodeFieldsWith :: (Monad m) => DecoderFun con m -> Int -> m (f a)

instance (DecodeFields val f, DecodeFields val g, CountFields g) => DecodeFields val (f :*: g) where
  decodeFieldsWith ctx index =
    (:*:)
      <$> decodeFieldsWith ctx index
      <*> decodeFieldsWith ctx (index + countFields (Proxy @g))

instance (Selector s, con a) => DecodeFields con (M1 S s (K1 i a)) where
  decodeFieldsWith ctx index = M1 . K1 <$> decoderFun ctx (getFieldName (selNameProxy (Proxy @s)) index)

instance DecodeFields val U1 where
  decodeFieldsWith _ _ = pure U1

getFieldName :: FieldName -> Int -> FieldName
getFieldName "" index = "_" <> show index
getFieldName label _ = label