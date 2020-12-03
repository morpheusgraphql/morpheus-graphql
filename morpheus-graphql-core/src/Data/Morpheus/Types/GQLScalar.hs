{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLScalar
  ( ScalarEncoder (..),
    ScalarDecoder (..),
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

scalarValidator :: forall f a. ScalarDecoder a => f a -> ScalarDefinition
scalarValidator _ = ScalarDefinition {validateValue = validator}
  where
    validator value = do
      scalarValue' <- toScalar value
      (_ :: a) <- parseValue scalarValue'
      pure value

-- | GraphQL Scalar Serializer
class ScalarEncoder (a :: *) where
  serialize :: a -> ScalarValue

-- | GraphQL Scalar Serializer
class ScalarDecoder (a :: *) where
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

instance ScalarDecoder Text where
  parseValue (String x) = pure x
  parseValue _ = Left ""

instance ScalarEncoder Text where
  serialize = String

instance ScalarDecoder Bool where
  parseValue (Boolean x) = pure x
  parseValue _ = Left ""

instance ScalarEncoder Bool where
  serialize = Boolean

instance ScalarDecoder Int where
  parseValue (Int x) = pure x
  parseValue _ = Left ""

instance ScalarEncoder Int where
  serialize = Int

instance ScalarDecoder Float where
  parseValue (Float x) = pure (double2Float x)
  parseValue (Int x) = pure $ fromInteger $ toInteger x
  parseValue _ = Left ""

instance ScalarEncoder Float where
  serialize = Float . float2Double

instance ScalarDecoder Double where
  parseValue (Float x) = pure x
  parseValue (Int x) = pure $ fromInteger $ toInteger x
  parseValue _ = Left ""

instance ScalarEncoder Double where
  serialize = Float

scalarToJSON :: ScalarEncoder a => a -> A.Value
scalarToJSON = A.toJSON . serialize

scalarFromJSON :: (Monad m, MonadFail m) => ScalarDecoder a => A.Value -> m a
scalarFromJSON x = case replaceValue x of
  Scalar value -> either (fail . unpack) pure (parseValue value)
  _ -> fail "input must be scalar value"
