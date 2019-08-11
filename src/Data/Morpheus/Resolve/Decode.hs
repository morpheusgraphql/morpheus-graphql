{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Resolve.Decode
  ( ArgumentsConstraint
  , decodeArguments
  ) where

import           Data.Proxy                                 (Proxy (..))
import           Data.Semigroup                             ((<>))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal               (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Kind                         (ENUM, GQL_KIND, INPUT_OBJECT, INPUT_UNION, SCALAR, WRAPPER)
import           Data.Morpheus.Resolve.Generics.EnumRep     (EnumRep (..))
import           Data.Morpheus.Types.GQLScalar              (GQLScalar (..), toScalar)
import           Data.Morpheus.Types.GQLType                (GQLType (KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Selection (Argument (..), Arguments)
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))

--
-- GENERIC UNION
--
class DecodeInputUnion f where
  decodeUnion :: Value -> Validation (f a)
  unionTags :: Proxy f -> [Text]

instance (Datatype c, DecodeInputUnion f) => DecodeInputUnion (M1 D c f) where
  decodeUnion = fmap M1 . decodeUnion
  unionTags _ = unionTags (Proxy @f)

instance (Constructor c, DecodeInputUnion f) => DecodeInputUnion (M1 C c f) where
  decodeUnion = fmap M1 . decodeUnion
  unionTags _ = unionTags (Proxy @f)

instance (Selector c, DecodeInputUnion f) => DecodeInputUnion (M1 S c f) where
  decodeUnion = fmap M1 . decodeUnion
  unionTags _ = unionTags (Proxy @f)

instance (DecodeInputUnion a, DecodeInputUnion b) => DecodeInputUnion (a :+: b) where
  decodeUnion (Object pairs) =
    case lookup "tag" pairs of
      Nothing -> internalArgumentError "tag not found on Input Union"
      Just (Enum name) ->
        case lookup name pairs of
          Nothing -> internalArgumentError ("type \"" <> name <> "\" was not provided on object")
          -- Decodes first Matching Union Type Value
          Just value
            | [name] == l1Tags -> L1 <$> decodeUnion value
          -- Decodes last Matching Union Type Value
          Just value
            | [name] == r1Tags -> R1 <$> decodeUnion value
          Just _
            -- JUMPS to Next Union Pair
            | name `elem` r1Tags -> R1 <$> decodeUnion (Object pairs)
          Just _ -> internalArgumentError ("type \"" <> name <> "\" could not find in union")
        where l1Tags = unionTags $ Proxy @a
              r1Tags = unionTags $ Proxy @b
      Just _ -> internalArgumentError "tag must be Enum"
  decodeUnion _ = internalArgumentError "Expected Input Object Union!"
  unionTags _ = unionTags (Proxy @a) ++ unionTags (Proxy @b)

instance (GQLType a, Decode a (KIND a)) => DecodeInputUnion (K1 i a) where
  decodeUnion value = K1 <$> decode value
  unionTags _ = [__typeName (Proxy @a)]

--
--  GENERIC INPUT OBJECT AND ARGUMENTS
--
type ArgumentsConstraint a = (Generic a, DecodeInputObject (Rep a))

decodeArguments :: (Generic p, DecodeInputObject (Rep p)) => Arguments -> Validation p
decodeArguments args = to <$> decodeObject (Object $ fmap (\(x, y) -> (x, argumentValue y)) args)

class DecodeInputObject f where
  decodeObject :: Value -> Validation (f a)

instance DecodeInputObject U1 where
  decodeObject _ = pure U1

type Sel s = M1 S s

proxySelName ::
     forall s. Selector s
  => Proxy (M1 S s)
  -> Text
proxySelName _ = pack $ selName (undefined :: M1 S s f a)

instance (Selector s, DecodeInputObject f) => DecodeInputObject (M1 S s f) where
  decodeObject (Object object) = M1 <$> selectFromObject
    where
      selectorName = proxySelName (Proxy @(Sel s))
      selectFromObject =
        case lookup selectorName object of
          Nothing    -> internalArgumentError ("Missing Field: " <> selectorName)
          Just value -> decodeObject value
  decodeObject isType = internalTypeMismatch "InputObject" isType

instance DecodeInputObject f => DecodeInputObject (M1 D c f) where
  decodeObject = fmap M1 . decodeObject

instance DecodeInputObject f => DecodeInputObject (M1 C c f) where
  decodeObject = fmap M1 . decodeObject

instance (DecodeInputObject f, DecodeInputObject g) => DecodeInputObject (f :*: g) where
  decodeObject gql = (:*:) <$> decodeObject gql <*> decodeObject gql

instance (Decode a (KIND a)) => DecodeInputObject (K1 i a) where
  decodeObject = fmap K1 . decode

decode ::
     forall a. Decode a (KIND a)
  => Value
  -> Validation a
decode = __decode (Proxy @(KIND a))

-- | Decode GraphQL query arguments and input values
class Decode a (b :: GQL_KIND) where
  __decode :: Proxy b -> Value -> Validation a

--
-- SCALAR
--
instance (GQLScalar a) => Decode a SCALAR where
  __decode _ value =
    case toScalar value >>= parseValue of
      Right scalar      -> return scalar
      Left errorMessage -> internalTypeMismatch errorMessage value

--
-- ENUM
--
instance (Generic a, EnumRep (Rep a)) => Decode a ENUM where
  __decode _ (Enum value) = to <$> decodeEnum value
  __decode _ isType       = internalTypeMismatch "Enum" isType

--
-- INPUT_OBJECT
--
instance (Generic a, DecodeInputObject (Rep a)) => Decode a INPUT_OBJECT where
  __decode _ (Object x) = to <$> decodeObject (Object x)
  __decode _ isType     = internalTypeMismatch "InputObject" isType

--
-- INPUT_UNION
--
instance (Generic a, DecodeInputUnion (Rep a)) => Decode a INPUT_UNION where
  __decode _ (Object x) = to <$> decodeUnion (Object x)
  __decode _ isType     = internalTypeMismatch "InputObject" isType

--
-- WRAPPERS: Maybe, List
--
instance Decode a (KIND a) => Decode (Maybe a) WRAPPER where
  __decode _ Null = pure Nothing
  __decode _ x    = Just <$> decode x

instance Decode a (KIND a) => Decode [a] WRAPPER where
  __decode _ (List li) = mapM decode li
  __decode _ isType    = internalTypeMismatch "List" isType
