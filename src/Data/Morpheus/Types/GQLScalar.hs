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

class GQLScalar a where
  parseValue :: ScalarValue -> Either Text a
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
