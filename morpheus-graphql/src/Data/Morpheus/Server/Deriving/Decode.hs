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
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( conNameProxy,
    selNameProxy,
    symbolName,
  )
import Data.Morpheus.Server.Deriving.Utils.Decode
  ( Context (..),
    DecoderT,
    Info (..),
    Tag (..),
    decodeFieldWith,
    getFieldName,
    handleEither,
    withInputObject,
    withInputUnion,
    withKind,
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
          { options = typeOptions (Proxy @a) defaultTypeOptions,
            contKind = D_CONS,
            typeName = deriveTypename (KindedProxy :: KindedProxy IN a)
          }

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

decideUnion ::
  (DecodeRep f1, DecodeRep f2) =>
  ([TypeName], [TypeName]) ->
  TypeName ->
  ValidValue ->
  DecoderT ((:+:) f1 f2 a)
decideUnion (left, right) name value
  | name `elem` left = L1 <$> decodeRep value
  | name `elem` right = R1 <$> decodeRep value
  | otherwise =
      throwError $
        internal $
          "Constructor \""
            <> msg name
            <> "\" could not find in Union"

traverseUnion ::
  (DecodeRep f, DecodeRep g) =>
  ([TypeName], [TypeName]) ->
  TypeName ->
  Object VALID ->
  ValidObject ->
  DecoderT ((f :+: g) a)
traverseUnion (l, r) name unions object
  | [name] == l = L1 <$> decodeRep (Object object)
  | [name] == r = R1 <$> decodeRep (Object object)
  | otherwise = decideUnion (l, r) name (Object unions)

getUnionInfos ::
  forall f a b.
  (DecodeRep a, DecodeRep b) =>
  f (a :+: b) ->
  DecoderT (Info, Info)
getUnionInfos _ =
  ( \context ->
      ( tags (Proxy @a) context,
        tags (Proxy @b) context
      )
  )
    <$> ask

class DecodeRep (f :: Type -> Type) where
  tags :: Proxy f -> Context -> Info
  decodeRep :: ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  tags _ = tags (Proxy @f)
  decodeRep value = M1 <$> decodeRep value

instance (DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  tags _ = tags (Proxy @a) <> tags (Proxy @b)
  decodeRep (Object obj) =
    do
      (left, right) <- getUnionInfos (Proxy @(a :+: b))
      withKind (kind (left <> right)) $ withInputUnion (traverseUnion (tagName left, tagName right)) obj
  decodeRep (Enum name) = do
    (left, right) <- getUnionInfos (Proxy @(a :+: b))
    decideUnion (tagName left, tagName right) name (Enum name)
  decodeRep _ = throwError (internal "lists and scalars are not allowed in Union")

instance (Constructor c, DecodeFields a) => DecodeRep (M1 C c a) where
  decodeRep = fmap M1 . decodeFields 0
  tags _ Context {typeName, options} = getTag (refType (Proxy @a))
    where
      getTag (Just memberRef)
        | isUnionRef memberRef = Info {kind = D_UNION, tagName = [memberRef]}
        | otherwise = Info {kind = D_CONS, tagName = [consName]}
      getTag Nothing = Info {kind = D_CONS, tagName = [consName]}
      --------
      consName = conNameProxy options (Proxy @c)
      ----------
      isUnionRef x = typeName <> x == consName

class DecodeFields (f :: Type -> Type) where
  refType :: Proxy f -> Maybe TypeName
  countFields :: Proxy f -> Int
  decodeFields :: Int -> ValidValue -> DecoderT (f a)

instance (DecodeFields f, DecodeFields g) => DecodeFields (f :*: g) where
  refType _ = Nothing
  countFields _ = countFields (Proxy @f) + countFields (Proxy @g)
  decodeFields index gql =
    (:*:)
      <$> decodeFields index gql
      <*> decodeFields (index + countFields (Proxy @g)) gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  countFields _ = 1
  refType _ = Just $ deriveTypename (KindedProxy :: KindedProxy IN a)
  decodeFields index value =
    M1 . K1 <$> do
      Context {options, contKind} <- ask
      case contKind of
        D_UNION -> lift (decode value)
        D_CONS ->
          let fieldName = getFieldName (selNameProxy options (Proxy @s)) index
              fieldDecoder = decodeFieldWith (lift . decode) fieldName
           in withInputObject fieldDecoder value

instance DecodeFields U1 where
  countFields _ = 0
  refType _ = Nothing
  decodeFields _ _ = pure U1
