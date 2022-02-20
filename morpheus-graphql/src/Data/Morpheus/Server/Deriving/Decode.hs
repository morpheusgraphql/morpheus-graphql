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
    Decode (..),
    DecodeConstraint,
  )
where

import Control.Monad.Except (MonadError (throwError))
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
  ( decodeFieldWith,
    handleEither,
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
    GQLTypeOptions (..),
    TypeData (..),
    __typeData,
    defaultTypeOptions,
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
    FieldName,
    GQLError,
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

type DecodeConstraint a = (DecodeKind (KIND a) a)

-- GENERIC
decodeArguments :: forall a. DecodeConstraint a => Arguments VALID -> ResolverState a
decodeArguments = decodeKind (Proxy @(KIND a)) . Object . fmap toEntry
  where
    toEntry Argument {..} = ObjectEntry argumentName argumentValue

-- | Decode GraphQL query arguments and input values
class Decode a where
  decode :: ValidValue -> ResolverState a

instance DecodeKind (KIND a) a => Decode a where
  decode = decodeKind (Proxy @(KIND a))

-- | Decode GraphQL type with Specific Kind
class DecodeKind (kind :: DerivingKind) a where
  decodeKind :: Proxy kind -> ValidValue -> ResolverState a

-- SCALAR
instance (DecodeScalar a, GQLType a) => DecodeKind SCALAR a where
  decodeKind _ = withScalar (gqlTypeName $ __typeData (KindedProxy :: KindedProxy LEAF a)) decodeScalar

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
            typeName = gqlTypeName $ __typeData (KindedProxy :: KindedProxy IN a)
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

-- data Input  =
--    InputHuman Human  -- direct link: { __typename: Human, Human: {field: ""} }
--   | InputRecord { name :: Text, age :: Int } -- { __typename: InputRecord, InputRecord: {field: ""} }
--   | IndexedType Int Text  -- { __typename: InputRecord, _0:2 , _1:""  }
--   | Zeus                 -- { __typename: Zeus }
--     deriving (Generic, GQLType)

decideUnion ::
  ( Functor m,
    MonadError GQLError m
  ) =>
  ([TypeName], value -> m (f1 a)) ->
  ([TypeName], value -> m (f2 a)) ->
  TypeName ->
  value ->
  m ((:+:) f1 f2 a)
decideUnion (left, f1) (right, f2) name value
  | name `elem` left =
    L1 <$> f1 value
  | name `elem` right =
    R1 <$> f2 value
  | otherwise =
    throwError
      $ internal
      $ "Constructor \""
        <> msg name
        <> "\" could not find in Union"

traverseUnion ::
  (DecodeRep f, DecodeRep g) =>
  ([TypeName], [TypeName]) ->
  TypeName ->
  Object VALID ->
  ValidObject ->
  DecoderT ((f :+: g) a)
traverseUnion (l1, r1) name unions object
  | [name] == l1 =
    L1 <$> decodeRep (Object object)
  | [name] == r1 =
    R1 <$> decodeRep (Object object)
  | otherwise = decideUnion (l1, decodeRep) (r1, decodeRep) name (Object unions)

data Tag = D_CONS | D_UNION deriving (Eq, Ord)

data Context = Context
  { contKind :: Tag,
    typeName :: TypeName,
    options :: GQLTypeOptions
  }

data Info = Info
  { kind :: Tag,
    tagName :: [TypeName]
  }

instance Semigroup Info where
  Info D_UNION t1 <> Info _ t2 = Info D_UNION (t1 <> t2)
  Info _ t1 <> Info D_UNION t2 = Info D_UNION (t1 <> t2)
  Info D_CONS t1 <> Info D_CONS t2 = Info D_CONS (t1 <> t2)

type DecoderT = ReaderT Context ResolverState

withKind :: Tag -> DecoderT a -> DecoderT a
withKind contKind = local (\ctx -> ctx {contKind})

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

--
-- GENERICS
--
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
      withKind (kind (left <> right)) $
        withInputUnion
          (traverseUnion (tagName left, tagName right))
          obj
  decodeRep (Enum name) = do
    (left, right) <- getUnionInfos (Proxy @(a :+: b))
    decideUnion
      (tagName left, decodeRep)
      (tagName right, decodeRep)
      name
      (Enum name)
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
    (:*:) <$> decodeFields index gql
      <*> decodeFields (index + countFields (Proxy @g)) gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  countFields _ = 1
  refType _ = Just $ gqlTypeName $ __typeData (KindedProxy :: KindedProxy IN a)
  decodeFields index value = M1 . K1 <$> do
    Context {options, contKind} <- ask
    case contKind of
      D_UNION -> lift (decode value)
      D_CONS ->
        let fieldName = getFieldName (selNameProxy options (Proxy @s)) index
            fieldDecoder = decodeFieldWith (lift . decode) fieldName
         in withInputObject fieldDecoder value

getFieldName :: FieldName -> Int -> FieldName
getFieldName "" index = "_" <> show index
getFieldName label _ = label

instance DecodeFields U1 where
  countFields _ = 0
  refType _ = Nothing
  decodeFields _ _ = pure U1
