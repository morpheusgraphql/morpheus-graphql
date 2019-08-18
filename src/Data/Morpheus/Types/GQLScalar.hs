{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Data.Morpheus.Types.GQLScalar
  ( GQLScalar(..)
  , toScalar
  ) where

import           Data.Morpheus.Types.Internal.Data  (DataValidator (..))
import           Data.Morpheus.Types.Internal.Value (ScalarValue (..), Value (..))
import           Data.Proxy                         (Proxy (..))
import           Data.Text                          (Text)

toScalar :: Value -> Either Text ScalarValue
toScalar (Scalar x) = pure x
toScalar _          = Left ""

-- | GraphQL Scalar
--
-- 'parseValue' and 'serialize' should be provided for every instances manually
class GQLScalar a
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
  --
  where
  parseValue :: ScalarValue -> Either Text a
  -- | serialization of haskell type into scalar value
  serialize :: a -> ScalarValue
  scalarValidator :: Proxy a -> DataValidator
  scalarValidator _ = DataValidator {validateValue = validator}
    where
      validator value = do
        scalarValue' <- toScalar value
        (_ :: a) <- parseValue scalarValue'
        return value

instance GQLScalar Text where
  parseValue (String x) = pure x
  parseValue _          = Left ""
  serialize = String

instance GQLScalar Bool where
  parseValue (Boolean x) = pure x
  parseValue _           = Left ""
  serialize = Boolean

instance GQLScalar Int where
  parseValue (Int x) = pure x
  parseValue _       = Left ""
  serialize = Int

instance GQLScalar Float where
  parseValue (Float x) = pure x
  parseValue _         = Left ""
  serialize = Float
