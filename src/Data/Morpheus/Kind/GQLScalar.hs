{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}

module Data.Morpheus.Kind.GQLScalar where

import           Data.Morpheus.Error.Internal            (internalTypeMismatch)
import           Data.Morpheus.Kind.GQLType              (GQLType (..), scalarTypeOf)
import           Data.Morpheus.Schema.TypeKind           (TypeKind (..))
import           Data.Morpheus.Types.Internal.Base       (Key)
import           Data.Morpheus.Types.Internal.Data       (DataField, DataScalarValidator (..), DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Internal.Value      (ScalarValue (..), Value (..))
import           Data.Proxy                              (Proxy (..))
import           Data.Text                               (Text)

toScalar :: Value -> Either Text ScalarValue
toScalar (Scalar x) = pure x
toScalar _          = Left "Scalar"

class GQLScalar a where
  parseValue :: ScalarValue -> Either Text a
  scalarValidator :: Proxy a -> DataScalarValidator
  scalarValidator _ = DataScalarValidator {validateValue = validator}
    where
      validator value = do
        scalarValue' <- toScalar value
        (_ :: a) <- parseValue scalarValue'
        return value
  decode :: Value -> Validation a
  decode value =
    case toScalar value >>= parseValue of
      Right scalar      -> return scalar
      Left errorMessage -> internalTypeMismatch errorMessage value
  serialize :: a -> ScalarValue
  encode :: a -> Value
  encode = Scalar . serialize
  asField :: GQLType a => Proxy a -> t -> Key -> DataField t
  asField _ = field_ SCALAR (Proxy @a)
  introspect :: GQLType a => Proxy a -> DataTypeLib -> DataTypeLib
  introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

instance GQLScalar Text where
  parseValue (String x) = pure x
  parseValue _          = Left "String"
  serialize = String

instance GQLScalar Bool where
  parseValue (Boolean x) = pure x
  parseValue _           = Left "Boolean"
  serialize = Boolean

instance GQLScalar Int where
  parseValue (Int x) = pure x
  parseValue _       = Left "Int"
  serialize = Int

instance GQLScalar Float where
  parseValue (Float x) = pure x
  parseValue _         = Left "Float"
  serialize = Float
