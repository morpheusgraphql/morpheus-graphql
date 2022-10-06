{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Decode
  ( decodeArguments,
    Decode,
    decode,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map as M
import Data.Morpheus.App.Internal.Resolving
  ( ResolverState,
  )
import Data.Morpheus.Server.Deriving.Schema.Directive (visitEnumName, visitFieldName)
import Data.Morpheus.Server.Deriving.Utils
  ( selNameProxy,
    symbolName,
  )
import Data.Morpheus.Server.Deriving.Utils.Decode
  ( Context (..),
    DecoderT,
    DescribeCons,
    DescribeFields (countFields),
    decodeFieldWith,
    getFieldName,
    getUnionInfos,
    handleEither,
    setVariantRef,
    withInputObject,
    withInputUnion,
    withScalar,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedProxy (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType
      ( KIND,
        typeOptions
      ),
    deriveTypename,
  )
import Data.Morpheus.Server.Types.Internal
  ( defaultTypeOptions,
  )
import Data.Morpheus.Server.Types.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Types.Types (Arg (Arg))
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
  )
import Data.Morpheus.Types.GQLWrapper
  ( DecodeWrapper (..),
    DecodeWrapperConstraint,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    IN,
    LEAF,
    Object,
    ObjectEntry (..),
    TypeName,
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    internal,
    msg,
  )
import GHC.Generics
import GHC.TypeLits (KnownSymbol)
import Relude

-- GENERIC
decodeArguments :: forall a. Decode a => Arguments VALID -> ResolverState a
decodeArguments = decode . Object . fmap toEntry
  where
    toEntry Argument {..} = ObjectEntry argumentName argumentValue

type Decode a = (DecodeKind (KIND a) a)

decode :: forall a. Decode a => ValidValue -> ResolverState a
decode = decodeKind (Proxy @(KIND a))

-- | Decode GraphQL type with Specific Kind
class DecodeKind (kind :: DerivingKind) a where
  decodeKind :: Proxy kind -> ValidValue -> ResolverState a

-- SCALAR
instance (DecodeScalar a, GQLType a) => DecodeKind SCALAR a where
  decodeKind _ = withScalar (deriveTypename (KindedProxy :: KindedProxy LEAF a)) decodeScalar

-- INPUT_OBJECT and  INPUT_UNION
instance
  ( Generic a,
    GQLType a,
    DecodeRep (Rep a)
  ) =>
  DecodeKind TYPE a
  where
  decodeKind _ = fmap to . (`runReaderT` context) . decodeRep
    where
      context =
        Context
          { options = typeOptions proxy defaultTypeOptions,
            isVariantRef = False,
            typeName = deriveTypename (KindedProxy :: KindedProxy IN a),
            enumVisitor = visitEnumName proxy,
            fieldVisitor = visitFieldName proxy
          }
        where
          proxy = Proxy @a

instance (Decode a, DecodeWrapperConstraint f a, DecodeWrapper f) => DecodeKind WRAPPER (f a) where
  decodeKind _ value =
    runExceptT (decodeWrapper decode value)
      >>= handleEither

instance (Decode a, KnownSymbol name) => DecodeKind CUSTOM (Arg name a) where
  decodeKind _ value = Arg <$> withInputObject fieldDecoder value
    where
      fieldDecoder = decodeFieldWith decode fieldName
      fieldName = symbolName (Proxy @name)

--  Map
instance (Ord k, Decode (k, v)) => DecodeKind CUSTOM (Map k v) where
  decodeKind _ v = M.fromList <$> (decode v :: ResolverState [(k, v)])

decideEither ::
  (DecodeRep f, DecodeRep g) =>
  ([TypeName], [TypeName]) ->
  TypeName ->
  ValidValue ->
  DecoderT ((f :+: g) a)
decideEither (left, right) name value
  | name `elem` left = L1 <$> decodeRep value
  | name `elem` right = R1 <$> decodeRep value
  | otherwise =
      throwError $
        internal $
          "Constructor \""
            <> msg name
            <> "\" could not find in Union"

decodeInputUnionObject ::
  (DecodeRep f, DecodeRep g) =>
  ([TypeName], [TypeName]) ->
  TypeName ->
  Object VALID ->
  ValidObject ->
  DecoderT ((f :+: g) a)
decodeInputUnionObject (l, r) name unions object
  | [name] == l = L1 <$> decodeRep (Object object)
  | [name] == r = R1 <$> decodeRep (Object object)
  | otherwise = decideEither (l, r) name (Object unions)

class DecodeRep (f :: Type -> Type) where
  decodeRep :: ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  decodeRep value = M1 <$> decodeRep value

instance (DescribeCons a, DescribeCons b, DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  decodeRep (Object obj) =
    do
      (kind, lr) <- getUnionInfos (Proxy @(a :+: b))
      setVariantRef kind $ withInputUnion (decodeInputUnionObject lr) obj
  decodeRep (Enum name) = do
    (_, (l, r)) <- getUnionInfos (Proxy @(a :+: b))
    visitor <- asks enumVisitor
    decideEither (map visitor l, map visitor r) name (Enum name)
  decodeRep _ = throwError (internal "lists and scalars are not allowed in Union")

instance (Constructor c, DecodeFields a) => DecodeRep (M1 C c a) where
  decodeRep = fmap M1 . decodeFields 0

class DecodeFields (f :: Type -> Type) where
  decodeFields :: Int -> ValidValue -> DecoderT (f a)

instance (DecodeFields f, DecodeFields g, DescribeFields g) => DecodeFields (f :*: g) where
  decodeFields index gql =
    (:*:)
      <$> decodeFields index gql
      <*> decodeFields (index + countFields (Proxy @g)) gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  decodeFields index value =
    M1 . K1 <$> do
      Context {options, isVariantRef, fieldVisitor} <- ask
      if isVariantRef
        then lift (decode value)
        else
          let fieldName = fieldVisitor $ getFieldName (selNameProxy options (Proxy @s)) index
              fieldDecoder = decodeFieldWith (lift . decode) fieldName
           in withInputObject fieldDecoder value

instance DecodeFields U1 where
  decodeFields _ _ = pure U1
