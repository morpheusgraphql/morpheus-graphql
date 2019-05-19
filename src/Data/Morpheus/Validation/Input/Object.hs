{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Input.Object
  ( validateInputValue
  ) where

import           Data.Morpheus.Error.Input            (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.Schema.Internal.AST    (Core (..), Field (..), GObject (..), InputField (..), InputType,
                                                       TypeLib (..), showFullAstType)
import qualified Data.Morpheus.Schema.Internal.AST    as T (InternalType (..))
import           Data.Morpheus.Types.JSType           (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.Query.Operator   (TypeWrapper (..))
import           Data.Morpheus.Validation.Input.Enum  (validateEnum)
import           Data.Morpheus.Validation.Utils.Utils (getInputType, lookupField)
import           Data.Text                            (Text)

typeMismatch :: JSType -> Text -> [Prop] -> InputError
typeMismatch jsType expected' path' = UnexpectedType path' expected' jsType

validateScalarTypes :: Text -> ScalarValue -> [Prop] -> InputValidation ScalarValue
validateScalarTypes "String" (String x)   = pure . const (String x)
validateScalarTypes "String" scalar       = Left . typeMismatch (Scalar scalar) "String"
validateScalarTypes "Int" (Int x)         = pure . const (Int x)
validateScalarTypes "Int" scalar          = Left . typeMismatch (Scalar scalar) "Int"
validateScalarTypes "Boolean" (Boolean x) = pure . const (Boolean x)
validateScalarTypes "Boolean" scalar      = Left . typeMismatch (Scalar scalar) "Boolean"
validateScalarTypes _ scalar              = pure . const scalar

-- Validate Variable Argument or all Possible input Values
validateInputValue :: TypeLib -> [Prop] -> [TypeWrapper] -> InputType -> (Text, JSType) -> InputValidation JSType
validateInputValue lib' prop' = validate
  where
    throwError wrappers' type' value' = Left $ UnexpectedType prop' (showFullAstType wrappers' type') value'
    {-- VALIDATION --}
    {-- 1. VALIDATE WRAPPERS -}
    validate :: [TypeWrapper] -> InputType -> (Text, JSType) -> InputValidation JSType
    -- throw error on not nullable type if value = null
    validate (NonNullType:wrappers') type' (_, JSNull) = throwError wrappers' type' JSNull
    -- resolves nullable value as null
    validate _ _ (_, JSNull) = return JSNull
    -- ignores NonNUllTypes if value /= null
    validate (NonNullType:wrappers') type' value' = validateInputValue lib' prop' wrappers' type' value'
    {-- VALIDATE LIST -}
    validate (ListType:wrappers') type' (key', JSList list') = JSList <$> mapM listCheck list'
      where
        listCheck element' = validateInputValue lib' prop' wrappers' type' (key', element')
    {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
    {-- VALIDATE OBJECT--}
    validate [] (T.Object (GObject parentFields' _)) (_, JSObject fields) = JSObject <$> mapM validateObject fields
      where
        validateObject (_name, value') = do
          (fieldTypeName', currentProp', error') <- validationData value'
          type' <- getInputType fieldTypeName' lib' error'
          wrappers' <- fieldTypeWrappers <$> getField
          value'' <- validateInputValue lib' currentProp' wrappers' type' (_name, value')
          return (_name, value'')
          where
            validationData x = do
              fieldTypeName' <- fieldType <$> getField
              let currentProp = prop' ++ [Prop _name fieldTypeName']
              let inputError = typeMismatch x fieldTypeName' currentProp
              return (fieldTypeName', currentProp, inputError)
            getField = unpackInputField <$> lookupField _name parentFields' (UnknownField prop' _name)
    {-- VALIDATE SCALAR --}
    validate [] (T.Enum tags core) (_, value') = validateEnum (UnexpectedType prop' (name core) value') tags value'
    {-- VALIDATE ENUM --}
    validate [] (T.Scalar core) (_, Scalar value') = Scalar <$> validateScalarTypes (name core) value' prop'
    {-- 3. THROW ERROR: on invalid values --}
    validate wrappers' type' (_, value') = throwError wrappers' type' value'
