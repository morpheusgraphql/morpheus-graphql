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

module Data.Morpheus.Execution.Server.Decode
  ( decodeArguments
  , Decode(..)
  , DecodeObject(..)
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( pack )
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalArgumentError
                                                , internalTypeMismatch
                                                )
import           Data.Morpheus.Execution.Internal.Decode
                                                ( decodeFieldWith
                                                , withEnum
                                                , withList
                                                , withMaybe
                                                , withObject
                                                , withUnion
                                                )
import           Data.Morpheus.Execution.Server.Generics.EnumRep
                                                ( EnumRep(..) )
import           Data.Morpheus.Kind             ( ENUM
                                                , GQL_KIND
                                                , INPUT_OBJECT
                                                , INPUT_UNION
                                                , SCALAR
                                                , AUTO
                                                )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar(..)
                                                , toScalar
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(KIND, __typeName) )
import           Data.Morpheus.Types.Internal.AST
                                                ( Key
                                                , Argument(..)
                                                , Arguments
                                                , Object
                                                , Value(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation )


-- | Decode GraphQL query arguments and input values
class Decode a where
  decode :: Value -> Validation a

instance {-# OVERLAPPABLE #-} DecodeKind (KIND a) a => Decode a where
  decode = decodeKind (Proxy @(KIND a))

instance Decode a => Decode (Maybe a) where
  decode = withMaybe decode

instance Decode a => Decode [a] where
  decode = withList decode

-- | Decode GraphQL type with Specific Kind
class DecodeKind (kind :: GQL_KIND) a where
  decodeKind :: Proxy kind -> Value -> Validation a

-- SCALAR
instance (GQLScalar a) => DecodeKind SCALAR a where
  decodeKind _ value = case toScalar value >>= parseValue of
    Right scalar       -> return scalar
    Left  errorMessage -> internalTypeMismatch errorMessage value

-- ENUM
instance (Generic a, EnumRep (Rep a)) => DecodeKind ENUM a where
  decodeKind _ = withEnum (fmap to . decodeEnum)

-- INPUT_UNION
-- TODO: FIXME
instance (Generic a, EnumRep (Rep a)) => DecodeKind AUTO a where
  decodeKind _ = withEnum (fmap to . decodeEnum)

-- INPUT_OBJECT
instance DecodeObject a => DecodeKind INPUT_OBJECT a where
  decodeKind _ = withObject decodeObject

-- INPUT_UNION
instance (Generic a, DecodeRep (Rep a)) => DecodeKind INPUT_UNION a where
  decodeKind _ = withObject (fmap to . decodeUnion)

-- GENERIC
decodeArguments :: DecodeObject p => Arguments -> Validation p
decodeArguments = decodeObject . fmap toObject
  where toObject (x, y) = (x, argumentValue y)

class DecodeObject a where
  decodeObject :: Object -> Validation a

instance {-# OVERLAPPABLE #-} (Generic a, DecodeRep (Rep a)) => DecodeObject a where
  decodeObject = fmap to . __decodeObject . Object

--
-- GENERICS
--
class DecodeRep f where
  tags :: Proxy f -> [Key]
  decodeRep :: Value -> Validation (f a)
  decodeUnion :: Object -> Validation (f a)
  __decodeObject :: Value -> Validation (f a)

instance DecodeRep U1 where
  tags _ = []
  __decodeObject _ = pure U1
  decodeUnion _ = pure U1

-- Recursive Decoding: (Selector (Rec1 ))
instance (Selector s, GQLType a, Decode a) => DecodeRep (M1 S s (K1 i a)) where
  tags _ = [__typeName (Proxy @a)]
  decodeUnion    = fmap (M1 . K1) . decode . Object
  __decodeObject = fmap (M1 . K1) . decodeRec
   where
    fieldName = pack $ selName (undefined :: M1 S s f a)
    decodeRec = withObject (decodeFieldWith decode fieldName)

instance (Datatype c, DecodeRep f) => DecodeRep (M1 D c f) where
  decodeUnion = fmap M1 . decodeUnion
  tags _ = tags (Proxy @f)
  __decodeObject = fmap M1 . __decodeObject

instance (Constructor c, DecodeRep f) => DecodeRep (M1 C c f) where
  decodeUnion = fmap M1 . decodeUnion
  tags _ = tags (Proxy @f)
  __decodeObject = fmap M1 . __decodeObject

instance (DecodeRep f, DecodeRep g) => DecodeRep (f :*: g) where
  __decodeObject gql = (:*:) <$> __decodeObject gql <*> __decodeObject gql

instance (DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  decodeUnion = withUnion handleUnion
   where
    handleUnion name unions object
      | [name] == l1Tags = L1 <$> decodeUnion object
      | [name] == r1Tags = R1 <$> decodeUnion object
      | name `elem` l1Tags = L1 <$> decodeUnion unions
      | name `elem` r1Tags = R1 <$> decodeUnion unions
      | otherwise = internalArgumentError
        ("type \"" <> name <> "\" could not find in union")
     where
      l1Tags = tags $ Proxy @a
      r1Tags = tags $ Proxy @b
  tags _ = tags (Proxy @a) ++ tags (Proxy @b)
