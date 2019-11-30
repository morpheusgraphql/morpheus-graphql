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
                                                , internalError
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
  decodeKind _ = fmap to . decodeRep

-- GENERIC
decodeArguments :: DecodeObject p => Arguments -> Validation p
decodeArguments = decodeObject . fmap toObject
  where toObject (x, y) = (x, argumentValue y)

class DecodeObject a where
  decodeObject :: Object -> Validation a

instance {-# OVERLAPPABLE #-} (Generic a, DecodeRep (Rep a)) => DecodeObject a where
  decodeObject = fmap to . decodeRep . Object

--
-- GENERICS
--
class DecodeRep f where
  enums :: Proxy f -> [Key]
  tags :: Proxy f -> [Key]
  decodeRep :: Value -> Validation (f a)

instance (Datatype d, DecodeRep f) => DecodeRep (M1 D d f) where
  enums _ = enums (Proxy @f)
  tags _ = tags (Proxy @f)
  decodeRep = fmap M1 . decodeRep

instance (Constructor c, DecodeRep f) => DecodeRep (M1 C c f) where
  enums _ = [pack $ conName (undefined :: (M1 C c U1 x))]
  tags _ = tags (Proxy @f)
  decodeRep = fmap M1 . decodeRep

instance (DecodeRep a, DecodeRep b) => DecodeRep (a :+: b) where
  enums _ = enums (Proxy @a) <> enums (Proxy @b)
  tags _ = tags (Proxy @a) <> tags (Proxy @b)
  decodeRep = __decode
   where
    __decode (Object object) = withUnion handleUnion object
    __decode (Enum name)
      | inLeft name
      = L1 <$> decodeRep (Enum name)
      | inRight name
      = R1 <$> decodeRep (Enum name)
      | otherwise
      = internalError $ "Constructor \"" <> name <> "\" could not find in Union"
    __decode _ = internalError "lists and scalars are not allowed in Union"
    -----------------------------------------------
    handleUnion name unions object
      | [name] == l1Tags = L1 <$> decodeRep (Object object)
      | [name] == r1Tags = R1 <$> decodeRep (Object object)
      | inLeft name = L1 <$> decodeRep (Object unions)
      | inRight name = R1 <$> decodeRep (Object unions)
      | otherwise = internalArgumentError
        ("type \"" <> name <> "\" could not find in union")
    ------------------------------------------------------- 
    l1Tags  = tags $ Proxy @a
    r1Tags  = tags $ Proxy @b
    inLeft  = (`elem` l1Tags)
    inRight = (`elem` r1Tags)

instance (DecodeRep f, DecodeRep g) => DecodeRep (f :*: g) where
  tags _ = []
  enums _ = []
  decodeRep gql = (:*:) <$> decodeRep gql <*> decodeRep gql

-- Recursive Decoding: (Selector (Rec1 ))
instance (Selector s, GQLType a, Decode a) => DecodeRep (M1 S s (K1 i a)) where
  tags _ = [__typeName (Proxy @a)]
  enums _ = []
 -- decodeRep    = fmap (M1 . K1) . decode . Object
  decodeRep = fmap (M1 . K1) . decodeRec
   where
    fieldName = pack $ selName (undefined :: M1 S s f a)
    decodeRec = withObject (decodeFieldWith decode fieldName)

instance DecodeRep U1 where
  tags _ = []
  enums = const []
  decodeRep _ = pure U1
