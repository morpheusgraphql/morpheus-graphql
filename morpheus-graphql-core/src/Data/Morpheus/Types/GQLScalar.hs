{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLScalar
  ( EncodeScalar (..),
    DecodeScalar (..),
    toScalar,
    scalarToJSON,
    scalarFromJSON,
    scalarValidator,
  )
where

import qualified Data.Aeson as A
import Data.Morpheus.Types.Internal.AST
  ( ScalarDefinition (..),
    ScalarValue (..),
    ValidValue,
    Value (..),
    replaceValue,
  )
import Data.Text (unpack)
import GHC.Float (double2Float, float2Double)
import Relude

toScalar :: ValidValue -> Either Text ScalarValue
toScalar (Scalar x) = pure x
toScalar _ = Left ""

scalarValidator :: forall f a. (DecodeScalar a) => f a -> ScalarDefinition
scalarValidator _ = ScalarDefinition {validateValue = validator}
  where
    validator value = do
      scalarValue' <- toScalar value
      (_ :: a) <- decodeScalar scalarValue'
      pure value

-- | GraphQL Scalar Serializer
class EncodeScalar (a :: Type) where
  encodeScalar :: a -> ScalarValue

-- | GraphQL Scalar parser
class DecodeScalar (a :: Type) where
  -- value parsing and validating
  --
  -- for exhaustive pattern matching  should be handled all scalar types : 'Int', 'Float', 'String', 'Boolean'
  --
  -- invalid values can be reported with 'Left' constructor :
  --
  -- @
  --   decodeScalar String _ = Left "" -- without error message
  --   -- or
  --   decodeScalar String _ = Left "Error Message"
  -- @
  decodeScalar :: ScalarValue -> Either Text a

instance DecodeScalar Text where
  decodeScalar (String x) = pure x
  decodeScalar _ = Left ""

instance EncodeScalar Text where
  encodeScalar = String

instance DecodeScalar Bool where
  decodeScalar (Boolean x) = pure x
  decodeScalar _ = Left ""

instance EncodeScalar Bool where
  encodeScalar = Boolean

instance DecodeScalar Int where
  decodeScalar (Int x) = pure x
  decodeScalar _ = Left ""

instance EncodeScalar Int where
  encodeScalar = Int

instance DecodeScalar Float where
  decodeScalar (Float x) = pure (double2Float x)
  decodeScalar (Int x) = pure $ fromInteger $ toInteger x
  decodeScalar _ = Left ""

instance EncodeScalar Float where
  encodeScalar = Float . float2Double

instance DecodeScalar Double where
  decodeScalar (Float x) = pure x
  decodeScalar (Int x) = pure $ fromInteger $ toInteger x
  decodeScalar _ = Left ""

instance EncodeScalar Double where
  encodeScalar = Float

scalarToJSON :: (EncodeScalar a) => a -> A.Value
scalarToJSON = A.toJSON . encodeScalar

scalarFromJSON :: (Monad m, MonadFail m) => (DecodeScalar a) => A.Value -> m a
scalarFromJSON x = case replaceValue x of
  Scalar value -> either (fail . unpack) pure (decodeScalar value)
  _ -> fail "input must be scalar value"
