{-# LANGUAGE ConstraintKinds       #-}
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

import           Data.Morpheus.Error.Internal               (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Kind                         (ENUM, INPUT_OBJECT, KIND, SCALAR, WRAPPER)
import           Data.Morpheus.Resolve.Generics.EnumRep     (EnumRep (..))
import           Data.Morpheus.Types.GQLScalar              (GQLScalar (..), toScalar)
import           Data.Morpheus.Types.Internal.AST.Selection (Argument (..), Arguments)
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

type Decode_ a = Value -> Validation a

type ArgumentsConstraint a = (Generic a, GDecode Arguments (Rep a))

decodeArguments :: (Generic p, GDecode Arguments (Rep p)) => Arguments -> Validation p
decodeArguments args = to <$> gDecode "" args

--
--  GENERIC
--
fixProxy :: (a -> f a) -> f a
fixProxy f = f undefined

class GDecode i f where
  gDecode :: Text -> i -> Validation (f a)

instance GDecode i U1 where
  gDecode _ _ = pure U1

instance (Selector c, GDecode i f) => GDecode i (M1 S c f) where
  gDecode _ gql = fixProxy (\x -> M1 <$> gDecode (pack $ selName x) gql)

instance (Datatype c, GDecode i f) => GDecode i (M1 D c f) where
  gDecode key gql = fixProxy $ const (M1 <$> gDecode key gql)

instance GDecode i f => GDecode i (M1 C c f) where
  gDecode meta gql = M1 <$> gDecode meta gql

instance (GDecode i f, GDecode i g) => GDecode i (f :*: g) where
  gDecode meta gql = (:*:) <$> gDecode meta gql <*> gDecode meta gql

instance (Decode a (KIND a)) => GDecode Value (K1 i a) where
  gDecode key' (Object object) =
    case lookup key' object of
      Nothing    -> internalArgumentError "Missing Argument"
      Just value -> K1 <$> decode value
  gDecode _ isType = internalTypeMismatch "InputObject" isType

instance Decode a (KIND a) => GDecode Arguments (K1 i a) where
  gDecode key' args =
    case lookup key' args of
      Nothing                -> internalArgumentError "Required Argument Not Found"
      Just (Argument x _pos) -> K1 <$> decode x

-- | Decode GraphQL query arguments and input values
decode ::
     forall a. Decode a (KIND a)
  => Decode_ a
decode = __decode (Proxy @(KIND a))

-- | Decode GraphQL query arguments and input values
class Decode a b where
  __decode :: Proxy b -> Decode_ a

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
  __decode _ (Enum value) = pure (to $ gToEnum value)
  __decode _ isType       = internalTypeMismatch "Enum" isType

--
-- INPUT_OBJECT
--
instance (Generic a, GDecode Value (Rep a)) => Decode a INPUT_OBJECT where
  __decode _ (Object x) = to <$> gDecode "" (Object x)
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
