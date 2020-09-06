{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.GQLScalar
  ( GQLScalar (..),
    toScalar,
    scalarToJSON,
    scalarFromJSON,
  )
where

import Control.Monad.Fail (MonadFail (..))
import qualified Data.Aeson as A
import Data.Either (Either (..), either)
import Data.Morpheus.Types.Internal.AST
  ( ScalarDefinition (..),
    ScalarValue (..),
    ValidValue,
    Value (..),
    replaceValue,
  )
import Data.Text (Text, unpack)
import Prelude
  ( ($),
    (.),
    Bool,
    Float,
    Int,
    Monad,
    fromInteger,
    pure,
    toInteger,
  )

toScalar :: ValidValue -> Either Text ScalarValue
toScalar (Scalar x) = pure x
toScalar _ = Left ""

-- | GraphQL Scalar
--
-- 'parseValue' and 'serialize' should be provided for every instances manually
class GQLScalar a where
  -- | value parsing and validating
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

  -- | serialization of haskell type into scalar value
  serialize :: a -> ScalarValue

  scalarValidator :: f a -> ScalarDefinition
  scalarValidator _ = ScalarDefinition {validateValue = validator}
    where
      validator value = do
        scalarValue' <- toScalar value
        (_ :: a) <- parseValue scalarValue'
        pure value

instance GQLScalar Text where
  parseValue (String x) = pure x
  parseValue _ = Left ""
  serialize = String

instance GQLScalar Bool where
  parseValue (Boolean x) = pure x
  parseValue _ = Left ""
  serialize = Boolean

instance GQLScalar Int where
  parseValue (Int x) = pure x
  parseValue _ = Left ""
  serialize = Int

instance GQLScalar Float where
  parseValue (Float x) = pure x
  parseValue (Int x) = pure $ fromInteger $ toInteger x
  parseValue _ = Left ""
  serialize = Float

scalarToJSON :: GQLScalar a => a -> A.Value
scalarToJSON = A.toJSON . serialize

scalarFromJSON :: (Monad m, MonadFail m) => GQLScalar a => A.Value -> m a
scalarFromJSON x = case replaceValue x of
  Scalar value -> either (fail . unpack) pure (parseValue value)
  _ -> fail "input must be scalar value"
