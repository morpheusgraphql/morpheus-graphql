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

import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Kind
  ( GQL_KIND,
    SCALAR,
    TYPE,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( conNameProxy,
    datatypeNameProxy,
    selNameProxy,
  )
import Data.Morpheus.Server.Internal.TH.Decode
  ( decodeFieldWith,
    withInputObject,
    withInputUnion,
    withList,
    withMaybe,
    withRefinedList,
    withScalar,
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
import Data.Morpheus.Types.GQLScalar
  ( GQLScalar (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    IN,
    InternalError,
    LEAF,
    Message,
    Object,
    ObjectEntry (..),
    TypeName (..),
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Failure (..),
    ResolverState,
  )
import Data.Morpheus.Utils.Kinded
  ( KindedProxy (..),
  )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics
import Relude

-- import Prelude (($), (-), (.), Either (Left, Right), Eq (..), Foldable (length), Ord, maybe, otherwise, show)

type DecodeConstraint a =
  ( Generic a,
    GQLType a,
    DecodeRep (Rep a)
  )

-- GENERIC
decodeArguments :: DecodeConstraint a => Arguments VALID -> ResolverState a
decodeArguments = decodeType . Object . fmap toEntry
  where
    toEntry Argument {..} = ObjectEntry argumentName argumentValue

-- | Decode GraphQL query arguments and input values
class Decode a where
  decode :: ValidValue -> ResolverState a

instance {-# OVERLAPPABLE #-} DecodeKind (KIND a) a => Decode a where
  decode = decodeKind (Proxy @(KIND a))

instance Decode a => Decode (Maybe a) where
  decode = withMaybe decode

instance Decode a => Decode [a] where
  decode = withList decode

instance Decode a => Decode (NonEmpty a) where
  decode = withRefinedList (maybe (Left "Expected a NonEmpty list") Right . NonEmpty.nonEmpty) decode

-- | Should this instance dedupe silently or fail on dupes ?
instance (Ord a, Decode a) => Decode (Set a) where
  decode val = do
    listVal <- withList (decode @a) val
    let setVal = Set.fromList listVal
    let setLength = length setVal
    let listLength = length setVal
    if listLength == setLength
      then pure setVal
      else failure (fromString ("Expected a List without duplicates, found " <> show (setLength - listLength) <> " duplicates") :: InternalError)

instance (Decode a) => Decode (Seq a) where
  decode = fmap Seq.fromList . withList decode

instance (Decode a) => Decode (Vector a) where
  decode = fmap Vector.fromList . withList decode

-- | Decode GraphQL type with Specific Kind
class DecodeKind (kind :: GQL_KIND) a where
  decodeKind :: Proxy kind -> ValidValue -> ResolverState a

-- SCALAR
instance (GQLScalar a, GQLType a) => DecodeKind SCALAR a where
  decodeKind _ = withScalar (gqlTypeName $ __typeData (KindedProxy :: KindedProxy LEAF a)) parseValue

-- INPUT_OBJECT and  INPUT_UNION
instance DecodeConstraint a => DecodeKind TYPE a where
  decodeKind _ = decodeType

decodeType :: forall a. DecodeConstraint a => ValidValue -> ResolverState a
decodeType = fmap to . (`runReaderT` context) . decodeRep
  where
    context =
      Context
        { options = typeOptions (Proxy @a) defaultTypeOptions,
          contKind = D_CONS,
          typeName = ""
        }

-- data Input  =
--    InputHuman Human  -- direct link: { __typename: Human, Human: {field: ""} }
--   | InputRecord { name :: Text, age :: Int } -- { __typename: InputRecord, InputRecord: {field: ""} }
--   | IndexedType Int Text  -- { __typename: InputRecord, _0:2 , _1:""  }
--   | Zeus                 -- { __typename: Zeus }
--     deriving (Generic, GQLType)

decideUnion ::
  ( Functor m,
    Failure Message m
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
    failure $
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

withTypeName :: TypeName -> DecoderT a -> DecoderT a
withTypeName typeName = local (\ctx -> ctx {typeName})

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
class DecodeRep (f :: * -> *) where
  tags :: Proxy f -> Context -> Info
  decodeRep :: ValidValue -> DecoderT (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  tags _ = tags (Proxy @f)
  decodeRep value =
    withTypeName
      (datatypeNameProxy (Proxy @d))
      (M1 <$> decodeRep value)

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
  decodeRep _ = failure ("lists and scalars are not allowed in Union" :: InternalError)

instance (Constructor c, DecodeFields a) => DecodeRep (M1 C c a) where
  decodeRep = fmap M1 . decodeFields
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

class DecodeFields (f :: * -> *) where
  refType :: Proxy f -> Maybe TypeName
  decodeFields :: ValidValue -> DecoderT (f a)

instance (DecodeFields f, DecodeFields g) => DecodeFields (f :*: g) where
  refType _ = Nothing
  decodeFields gql = (:*:) <$> decodeFields gql <*> decodeFields gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  refType _ = Just $ gqlTypeName $ __typeData (KindedProxy :: KindedProxy IN a)
  decodeFields value = M1 . K1 <$> do
    Context {options, contKind} <- ask
    if contKind == D_UNION
      then lift (decode value)
      else
        let fieldName = selNameProxy options (Proxy @s)
            fieldDecoder = decodeFieldWith (lift . decode) fieldName
         in withInputObject fieldDecoder value

instance DecodeFields U1 where
  refType _ = Nothing
  decodeFields _ = pure U1
