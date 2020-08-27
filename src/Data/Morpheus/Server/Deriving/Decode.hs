{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- MORPHEUS
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Decode
  ( decodeArguments,
    Decode (..),
    DecodeType (..),
  )
where

import Control.Applicative ((<*>), pure)
import Control.Monad ((>>=))
import Data.Functor ((<$>), Functor (..))
import Data.List (elem)
import Data.Maybe (Maybe (..))
import Data.Morpheus.Internal.Utils
  ( elems,
    stripNamespace,
  )
import Data.Morpheus.Kind
  ( ENUM,
    GQL_KIND,
    INPUT,
    OUTPUT,
    SCALAR,
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
    withScalar,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType
      ( KIND,
        __typeName,
        hasNamespace
      ),
  )
import Data.Morpheus.Types.GQLScalar
  ( GQLScalar (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    InternalError,
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
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.Generics
import Prelude
  ( ($),
    (.),
    Eq (..),
    Ord,
    otherwise,
  )

-- GENERIC
decodeArguments :: DecodeType a => Arguments VALID -> ResolverState a
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

-- | Decode GraphQL type with Specific Kind
class DecodeKind (kind :: GQL_KIND) a where
  decodeKind :: Proxy kind -> ValidValue -> ResolverState a

-- SCALAR
instance (GQLScalar a, GQLType a) => DecodeKind SCALAR a where
  decodeKind _ = withScalar (__typeName (Proxy @a)) parseValue

-- ENUM
instance DecodeType a => DecodeKind ENUM a where
  decodeKind _ = decodeType

-- TODO: remove
instance DecodeType a => DecodeKind OUTPUT a where
  decodeKind _ = decodeType

-- INPUT_OBJECT and  INPUT_UNION
instance DecodeType a => DecodeKind INPUT a where
  decodeKind _ = decodeType

class DecodeType a where
  decodeType :: ValidValue -> ResolverState a

instance
  {-# OVERLAPPABLE #-}
  ( Generic a,
    GQLType a,
    DecodeRep (Rep a)
  ) =>
  DecodeType a
  where
  decodeType = fmap to . decodeRep . (hasNamespace (Proxy @a),,Cont D_CONS "")

-- data Input  =
--    InputHuman Human  -- direct link: { __typename: Human, Human: {field: ""} }
--   | InputRecord { name :: Text, age :: Int } -- { __typename: InputRecord, InputRecord: {field: ""} }
--   | IndexedType Int Text  -- { __typename: InputRecord, _0:2 , _1:""  }
--   | Zeus                 -- { __typename: Zeus }
--     deriving (Generic, GQLType)

decideUnion ::
  ([TypeName], value -> ResolverState (f1 a)) ->
  ([TypeName], value -> ResolverState (f2 a)) ->
  TypeName ->
  value ->
  ResolverState ((:+:) f1 f2 a)
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

data Tag = D_CONS | D_UNION deriving (Eq, Ord)

data Cont = Cont
  { contKind :: Tag,
    typeName :: TypeName
  }

data Info = Info
  { kind :: Tag,
    tagName :: [TypeName]
  }

instance Semigroup Info where
  Info D_UNION t1 <> Info _ t2 = Info D_UNION (t1 <> t2)
  Info _ t1 <> Info D_UNION t2 = Info D_UNION (t1 <> t2)
  Info D_CONS t1 <> Info D_CONS t2 = Info D_CONS (t1 <> t2)

--
-- GENERICS
--
class DecodeRep f where
  tags :: Proxy f -> TypeName -> Info
  decodeRep :: (Maybe TypeName, ValidValue, Cont) -> ResolverState (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  tags _ = tags (Proxy @f)
  decodeRep (ns, x, y) =
    M1
      <$> decodeRep
        (ns, x, y {typeName = datatypeNameProxy (Proxy @d)})

getEnumTag :: ValidObject -> ResolverState TypeName
getEnumTag x = case elems x of
  [ObjectEntry "enum" (Enum value)] -> pure value
  _ -> failure ("bad union enum object" :: InternalError)

instance (DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  tags _ = tags (Proxy @a) <> tags (Proxy @b)
  decodeRep = __decode
    where
      __decode (ns, Object obj, cont) = withInputUnion handleUnion obj
        where
          handleUnion name unions object
            | name == typeName cont <> "EnumObject" =
              getEnumTag object >>= __decode . (ns,,ctx) . Enum
            | [name] == l1 =
              L1 <$> decodeRep (ns, Object object, ctx)
            | [name] == r1 =
              R1 <$> decodeRep (ns, Object object, ctx)
            | otherwise =
              decideUnion (l1, decodeRep) (r1, decodeRep) name (ns, Object unions, ctx)
          l1 = tagName l1t
          r1 = tagName r1t
          l1t = tags (Proxy @a) (typeName cont)
          r1t = tags (Proxy @b) (typeName cont)
          ctx = cont {contKind = kind (l1t <> r1t)}
      __decode (ns, Enum name, cxt) =
        decideUnion
          (tagName $ tags (Proxy @a) (typeName cxt), decodeRep)
          (tagName $ tags (Proxy @b) (typeName cxt), decodeRep)
          name
          (ns, Enum name, cxt)
      __decode _ = failure ("lists and scalars are not allowed in Union" :: InternalError)

instance (Constructor c, DecodeFields a) => DecodeRep (M1 C c a) where
  decodeRep = fmap M1 . decodeFields
  tags _ baseName = getTag (refType (Proxy @a))
    where
      getTag (Just memberRef)
        | isUnionRef memberRef = Info {kind = D_UNION, tagName = [memberRef]}
        | otherwise = Info {kind = D_CONS, tagName = [consName]}
      getTag Nothing = Info {kind = D_CONS, tagName = [consName]}
      --------
      consName = conNameProxy (Proxy @c)
      ----------
      isUnionRef x = baseName <> x == consName

class DecodeFields f where
  refType :: Proxy f -> Maybe TypeName
  decodeFields :: (Maybe TypeName, ValidValue, Cont) -> ResolverState (f a)

instance (DecodeFields f, DecodeFields g) => DecodeFields (f :*: g) where
  refType _ = Nothing
  decodeFields gql = (:*:) <$> decodeFields gql <*> decodeFields gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  refType _ = Just $ __typeName (Proxy @a)
  decodeFields (namespace, value, Cont {contKind})
    | contKind == D_UNION = M1 . K1 <$> decode value
    | otherwise = __decode value
    where
      __decode = fmap (M1 . K1) . decodeRec
      fieldName = stripNamespace namespace $ selNameProxy (Proxy @s)
      decodeRec = withInputObject (decodeFieldWith decode fieldName)

instance DecodeFields U1 where
  refType _ = Nothing
  decodeFields _ = pure U1
