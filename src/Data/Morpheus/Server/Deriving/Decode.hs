{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Decode
  ( decodeArguments,
    Decode (..),
    DecodeType (..),
  )
where

-- MORPHEUS
import Data.Morpheus.Error
  ( internalError,
    internalTypeMismatch,
  )
import Data.Morpheus.Internal.Utils
  ( elems,
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
    withList,
    withMaybe,
    withObject,
    withUnion,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (KIND, __typeName))
import Data.Morpheus.Types.GQLScalar
  ( GQLScalar (..),
    toScalar,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    ObjectEntry (..),
    TypeName (..),
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Failure (..),
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import GHC.Generics

-- GENERIC
decodeArguments :: DecodeType a => Arguments VALID -> Eventless a
decodeArguments = decodeType . Object . fmap toEntry
  where
    toEntry (Argument name value _) = ObjectEntry name value

-- | Decode GraphQL query arguments and input values
class Decode a where
  decode :: ValidValue -> Eventless a

instance {-# OVERLAPPABLE #-} DecodeKind (KIND a) a => Decode a where
  decode = decodeKind (Proxy @(KIND a))

instance Decode a => Decode (Maybe a) where
  decode = withMaybe decode

instance Decode a => Decode [a] where
  decode = withList decode

-- | Decode GraphQL type with Specific Kind
class DecodeKind (kind :: GQL_KIND) a where
  decodeKind :: Proxy kind -> ValidValue -> Eventless a

-- SCALAR
instance (GQLScalar a) => DecodeKind SCALAR a where
  decodeKind _ value = case toScalar value >>= parseValue of
    Right scalar -> return scalar
    Left errorMessage -> internalTypeMismatch (msg errorMessage) value

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
  decodeType :: ValidValue -> Eventless a

instance {-# OVERLAPPABLE #-} (Generic a, DecodeRep (Rep a)) => DecodeType a where
  decodeType = fmap to . decodeRep . (,Cont D_CONS "")

-- data Inpuz  =
--    InputHuman Human  -- direct link: { __typename: Human, Human: {field: ""} }
--   | InputRecord { name :: Text, age :: Int } -- { __typename: InputRecord, InputRecord: {field: ""} }
--   | IndexedType Int Text  -- { __typename: InputRecord, _0:2 , _1:""  }
--   | Zeus                 -- { __typename: Zeus }
--     deriving (Generic, GQLType)

decideUnion ::
  ([TypeName], value -> Eventless (f1 a)) ->
  ([TypeName], value -> Eventless (f2 a)) ->
  TypeName ->
  value ->
  Eventless ((:+:) f1 f2 a)
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
  decodeRep :: (ValidValue, Cont) -> Eventless (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  tags _ = tags (Proxy @f)
  decodeRep (x, y) =
    M1
      <$> decodeRep
        (x, y {typeName = datatypeNameProxy (Proxy @d)})

getEnumTag :: ValidObject -> Eventless TypeName
getEnumTag x = case elems x of
  [ObjectEntry "enum" (Enum value)] -> pure value
  _ -> internalError "bad union enum object"

instance (DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  tags _ = tags (Proxy @a) <> tags (Proxy @b)
  decodeRep = __decode
    where
      __decode (Object obj, cont) = withUnion handleUnion obj
        where
          handleUnion name unions object
            | name == typeName cont <> "EnumObject" =
              getEnumTag object >>= __decode . (,ctx) . Enum
            | [name] == l1 =
              L1 <$> decodeRep (Object object, ctx)
            | [name] == r1 =
              R1 <$> decodeRep (Object object, ctx)
            | otherwise =
              decideUnion (l1, decodeRep) (r1, decodeRep) name (Object unions, ctx)
          l1 = tagName l1t
          r1 = tagName r1t
          l1t = tags (Proxy @a) (typeName cont)
          r1t = tags (Proxy @b) (typeName cont)
          ctx = cont {contKind = kind (l1t <> r1t)}
      __decode (Enum name, cxt) =
        decideUnion
          (tagName $ tags (Proxy @a) (typeName cxt), decodeRep)
          (tagName $ tags (Proxy @b) (typeName cxt), decodeRep)
          name
          (Enum name, cxt)
      __decode _ = internalError "lists and scalars are not allowed in Union"

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
  decodeFields :: (ValidValue, Cont) -> Eventless (f a)

instance (DecodeFields f, DecodeFields g) => DecodeFields (f :*: g) where
  refType _ = Nothing
  decodeFields gql = (:*:) <$> decodeFields gql <*> decodeFields gql

instance (Selector s, GQLType a, Decode a) => DecodeFields (M1 S s (K1 i a)) where
  refType _ = Just $ __typeName (Proxy @a)
  decodeFields (value, Cont {contKind})
    | contKind == D_UNION = M1 . K1 <$> decode value
    | otherwise = __decode value
    where
      __decode = fmap (M1 . K1) . decodeRec
      fieldName = selNameProxy (Proxy @s)
      decodeRec = withObject (decodeFieldWith decode fieldName)

instance DecodeFields U1 where
  refType _ = Nothing
  decodeFields _ = pure U1
