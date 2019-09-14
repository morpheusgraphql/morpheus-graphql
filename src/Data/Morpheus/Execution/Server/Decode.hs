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
  ( decodeArguments
  , Decode(..)
  , DecodeObject(..)
  ) where

import           Data.Proxy                                      (Proxy (..))
import           Data.Semigroup                                  ((<>))
import           Data.Text                                       (Text, pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal                    (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Execution.Internal.Decode         (decodeFieldWith, withEnum, withList, withMaybe,
                                                                  withObject, withUnion)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (ENUM, GQL_KIND, INPUT_OBJECT, INPUT_UNION, SCALAR)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..), toScalar)
import           Data.Morpheus.Types.GQLType                     (GQLType (KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Selection      (Argument (..), Arguments)
import           Data.Morpheus.Types.Internal.Validation         (Validation)
import           Data.Morpheus.Types.Internal.Value              (Object, Value (..))

-- | Decode GraphQL query arguments and input values
class Decode a where
  decode :: Value -> Validation a
  default decode :: Decode1 a (KIND a) =>
    Value -> Validation a
  decode = __decode (Proxy @(KIND a))

instance {-# OVERLAPPABLE #-} Decode1 a (KIND a) => Decode a

instance Decode a => Decode (Maybe a) where
  decode = withMaybe decode

instance Decode a => Decode [a] where
  decode = withList decode

-- Decode1
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
  __decode _ = withEnum (fmap to . decodeEnum)

-- INPUT_OBJECT
instance DecodeObject a => Decode1 a INPUT_OBJECT where
  __decode _ = withObject decodeObject

-- INPUT_UNION
instance (Generic a, DecodeInput (Rep a)) => Decode1 a INPUT_UNION where
  __decode _ = withObject (fmap to . decodeUnion)

-- GENERIC
decodeArguments :: DecodeObject p => Arguments -> Validation p
decodeArguments args = decodeObject (fmap (\(x, y) -> (x, argumentValue y)) args)

class DecodeObject a where
  decodeObject :: Object -> Validation a
  default decodeObject :: (Generic a, DecodeInput (Rep a)) =>
    Object -> Validation a
  decodeObject = fmap to . __decodeObject . Object

instance {-# OVERLAPPABLE #-} (Generic a, DecodeInput (Rep a)) => DecodeObject a

--
-- GENERICS
--
class DecodeInput f where
  decodeUnion :: Object -> Validation (f a)
  __decodeObject :: Value -> Validation (f a)
  unionTags :: Proxy f -> [Text]

instance DecodeInput U1 where
  __decodeObject _ = pure U1
  decodeUnion _ = pure U1
  unionTags _ = []

instance (Datatype c, DecodeInput f) => DecodeInput (M1 D c f) where
  decodeUnion = fmap M1 . decodeUnion
  unionTags _ = unionTags (Proxy @f)
  __decodeObject = fmap M1 . __decodeObject

instance (Selector s, DecodeInput f) => DecodeInput (M1 S s f) where
  decodeUnion = fmap M1 . decodeUnion
  unionTags _ = unionTags (Proxy @f)
  __decodeObject = fmap M1 . withObject (decodeFieldWith __decodeObject fieldName)
    where
      fieldName = pack $ selName (undefined :: M1 S s f a)

instance (Constructor c, DecodeInput f) => DecodeInput (M1 C c f) where
  decodeUnion = fmap M1 . decodeUnion
  unionTags _ = unionTags (Proxy @f)
  __decodeObject = fmap M1 . __decodeObject

instance (DecodeInput f, DecodeInput g) => DecodeInput (f :*: g) where
  __decodeObject gql = (:*:) <$> __decodeObject gql <*> __decodeObject gql

instance (GQLType a, Decode a) => DecodeInput (K1 i a) where
  decodeUnion = fmap K1 . decode . Object
  unionTags _ = [__typeName (Proxy @a)]
  __decodeObject = fmap K1 . decode

instance (DecodeInput a, DecodeInput b) => DecodeInput (a :+: b) where
  decodeUnion = withUnion handleUnion
    where
      handleUnion name unions object
        | [name] == l1Tags = L1 <$> decodeUnion object
        | [name] == r1Tags = R1 <$> decodeUnion object
        | name `elem` l1Tags = L1 <$> decodeUnion unions
        | name `elem` r1Tags = R1 <$> decodeUnion unions
        | otherwise = internalArgumentError ("type \"" <> name <> "\" could not find in union")
        where
          l1Tags = unionTags $ Proxy @a
          r1Tags = unionTags $ Proxy @b
  unionTags _ = unionTags (Proxy @a) ++ unionTags (Proxy @b)
