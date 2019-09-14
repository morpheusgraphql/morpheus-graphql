{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Execution.Server.Decode
  ( ArgumentsConstraint
  , decodeArguments
  , Decode(..)
  , DecodeObject(..)
  ) where

import           Data.Proxy                                      (Proxy (..))
import           Data.Semigroup                                  ((<>))
import           Data.Text                                       (Text, pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal                    (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Execution.Internal.Decode         (decodeFieldWith, withObject)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (ENUM, GQL_KIND, INPUT_OBJECT, INPUT_UNION, SCALAR,
                                                                  WRAPPER)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..), toScalar)
import           Data.Morpheus.Types.GQLType                     (GQLType (KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Selection      (Argument (..), Arguments)
import           Data.Morpheus.Types.Internal.Validation         (Validation)
import           Data.Morpheus.Types.Internal.Value              (Object, Value (..))

--
-- GENERIC UNION
--
class DecodeInputUnion f where
  decodeUnion :: Object -> Validation (f a)
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
  decodeUnion pairs =
    case lookup "tag" pairs of
      Nothing -> internalArgumentError "tag not found on Input Union"
      Just (Enum name) ->
        case lookup name pairs of
          Nothing -> internalArgumentError ("type \"" <> name <> "\" was not provided on object")
          Just value
          -- Decodes first Matching Union Type Value
            | [name] == l1Tags -> L1 <$> withObject decodeUnion value
          -- Decodes last Matching Union Type Value
            | [name] == r1Tags -> R1 <$> withObject decodeUnion value
          -- JUMPS to Next Union Pair
            | name `elem` r1Tags -> R1 <$> decodeUnion pairs
            | otherwise -> internalArgumentError ("type \"" <> name <> "\" could not find in union")
        where l1Tags = unionTags $ Proxy @a
              r1Tags = unionTags $ Proxy @b
      Just _ -> internalArgumentError "tag must be Enum"
  unionTags _ = unionTags (Proxy @a) ++ unionTags (Proxy @b)

instance (GQLType a, Decode a) => DecodeInputUnion (K1 i a) where
  decodeUnion = fmap K1 . decode . Object
  unionTags _ = [__typeName (Proxy @a)]

--
--  GENERIC INPUT OBJECT AND ARGUMENTS
--
type ArgumentsConstraint a = (Generic a, DecodeObject a)

decodeArguments :: (Generic p, DecodeObject p) => Arguments -> Validation p
decodeArguments args = decodeObject (fmap (\(x, y) -> (x, argumentValue y)) args)

class DecodeObject a where
  decodeObject :: Object -> Validation a
  default decodeObject :: (Generic a, DecodeInputObject (Rep a)) =>
    Object -> Validation a
  decodeObject = fmap to . __decodeObject . Object

instance {-# OVERLAPPABLE #-} (Generic a, DecodeInputObject (Rep a)) => DecodeObject a

class DecodeInputObject f where
  __decodeObject :: Value -> Validation (f a)

instance DecodeInputObject U1 where
  __decodeObject _ = pure U1

instance (Selector s, DecodeInputObject f) => DecodeInputObject (M1 S s f) where
  __decodeObject = fmap M1 . withObject (decodeFieldWith __decodeObject fieldName)
    where
      fieldName = pack $ selName (undefined :: M1 S s f a)

instance DecodeInputObject f => DecodeInputObject (M1 D c f) where
  __decodeObject = fmap M1 . __decodeObject

instance DecodeInputObject f => DecodeInputObject (M1 C c f) where
  __decodeObject = fmap M1 . __decodeObject

instance (DecodeInputObject f, DecodeInputObject g) => DecodeInputObject (f :*: g) where
  __decodeObject gql = (:*:) <$> __decodeObject gql <*> __decodeObject gql

instance Decode a => DecodeInputObject (K1 i a) where
  __decodeObject = fmap K1 . decode

-- | Decode GraphQL query arguments and input values
class Decode a where
  decode :: Value -> Validation a
  default decode :: Decode1 a (KIND a) =>
    Value -> Validation a
  decode = __decode (Proxy @(KIND a))

instance {-# OVERLAPPABLE #-} Decode1 a (KIND a) => Decode a

-- | Decode GraphQL query arguments and input values
class Decode1 a (b :: GQL_KIND) where
  __decode :: Proxy b -> Value -> Validation a

-- SCALAR
instance (GQLScalar a) => Decode1 a SCALAR where
  __decode _ value =
    case toScalar value >>= parseValue of
      Right scalar      -> return scalar
      Left errorMessage -> internalTypeMismatch errorMessage value

-- ENUM
instance (Generic a, EnumRep (Rep a)) => Decode1 a ENUM where
  __decode _ (Enum value) = to <$> decodeEnum value
  __decode _ isType       = internalTypeMismatch "Enum" isType

-- INPUT_OBJECT
instance DecodeObject a => Decode1 a INPUT_OBJECT where
  __decode _ = withObject decodeObject

-- INPUT_UNION
instance (Generic a, DecodeInputUnion (Rep a)) => Decode1 a INPUT_UNION where
  __decode _ = withObject (fmap to . decodeUnion)

-- WRAPPERS: Maybe, List
instance Decode a => Decode1 (Maybe a) WRAPPER where
  __decode _ Null = pure Nothing
  __decode _ x    = Just <$> decode x

instance Decode a => Decode1 [a] WRAPPER where
  __decode _ (List li) = mapM decode li
  __decode _ isType    = internalTypeMismatch "List" isType
