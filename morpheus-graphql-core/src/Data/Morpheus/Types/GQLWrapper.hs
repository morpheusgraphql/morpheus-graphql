{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLWrapper
  ( WrapperEncoder (..),
    WrapperDecoder (..),
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

scalarValidator :: forall f a. WrapperDecoder a => f a -> ScalarDefinition
scalarValidator _ = ScalarDefinition {validateValue = validator}
  where
    validator value = do
      scalarValue' <- toScalar value
      (_ :: a) <- parseValue scalarValue'
      pure value

-- | GraphQL Scalar Serializer
class WrapperEncoder (a :: *) where
  serialize :: a -> ScalarValue

-- | GraphQL Scalar Serializer
class WrapperDecoder (a :: *) where
  -- value parsing and validating
  --
  -- for exhaustive pattern matching  should be handled all scalar types : 'Int', 'Float', 'String', 'Boolean'
  --
  -- invalid values can be reported with 'Left' constructor :
  --
  -- @
  --   parseValue String _ = Left "" -- without error message
  --   -- or
  --   parseValue String _ = Left "Error Message"
  -- @
  parseValue :: ScalarValue -> Either Text a

instance WrapperDecoder Text where
  parseValue (String x) = pure x
  parseValue _ = Left ""

instance WrapperEncoder Text where
  serialize = String

instance WrapperDecoder Bool where
  parseValue (Boolean x) = pure x
  parseValue _ = Left ""

instance WrapperEncoder Bool where
  serialize = Boolean

instance WrapperDecoder Int where
  parseValue (Int x) = pure x
  parseValue _ = Left ""

instance WrapperEncoder Int where
  serialize = Int

instance WrapperDecoder Float where
  parseValue (Float x) = pure (double2Float x)
  parseValue (Int x) = pure $ fromInteger $ toInteger x
  parseValue _ = Left ""

instance WrapperEncoder Float where
  serialize = Float . float2Double

instance WrapperDecoder Double where
  parseValue (Float x) = pure x
  parseValue (Int x) = pure $ fromInteger $ toInteger x
  parseValue _ = Left ""

instance WrapperEncoder Double where
  serialize = Float

scalarToJSON :: WrapperEncoder a => a -> A.Value
scalarToJSON = A.toJSON . serialize

scalarFromJSON :: (Monad m, MonadFail m) => WrapperDecoder a => A.Value -> m a
scalarFromJSON x = case replaceValue x of
  Scalar value -> either (fail . unpack) pure (parseValue value)
  _ -> fail "input must be scalar value"
