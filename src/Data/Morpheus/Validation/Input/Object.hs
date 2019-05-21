{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Input.Object
  ( validateInputValue
  ) where

import           Data.Morpheus.Error.Input            (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.Types.Internal.Data    (DataField (..), DataInputType, DataKind (..), DataType (..),
                                                       DataTypeLib (..), DataTypeWrapper (..), DataValidator (..),
                                                       showFullAstType)
import           Data.Morpheus.Types.Internal.Value   (Value (..))
import           Data.Morpheus.Validation.Input.Enum  (validateEnum)
import           Data.Morpheus.Validation.Utils.Utils (getInputType, lookupField)
import           Data.Text                            (Text)

typeMismatch :: Value -> Text -> [Prop] -> InputError
typeMismatch jsType expected' path' = UnexpectedType path' expected' jsType

-- Validate Variable Argument or all Possible input Values
validateInputValue ::
     DataTypeLib -> [Prop] -> [DataTypeWrapper] -> DataInputType -> (Text, Value) -> InputValidation Value
validateInputValue lib' prop' = validate
  where
    throwError :: [DataTypeWrapper] -> DataInputType -> Value -> InputValidation Value
    throwError wrappers' type' value' = Left $ UnexpectedType prop' (showFullAstType wrappers' type') value'
    {-- VALIDATION --}
    {-- 1. VALIDATE WRAPPERS -}
    validate :: [DataTypeWrapper] -> DataInputType -> (Text, Value) -> InputValidation Value
    -- throw error on not nullable type if value = null
    validate (NonNullType:wrappers') type' (_, Null) = throwError wrappers' type' Null
    -- resolves nullable value as null
    validate _ _ (_, Null) = return Null
    -- ignores NonNUllTypes if value /= null
    validate (NonNullType:wrappers') type' value' = validateInputValue lib' prop' wrappers' type' value'
    {-- VALIDATE LIST -}
    validate (ListType:wrappers') type' (key', List list') = List <$> mapM validateElement list'
      where
        validateElement element' = validateInputValue lib' prop' wrappers' type' (key', element')
    {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
    {-- VALIDATE OBJECT--}
    validate [] (ObjectKind DataType {typeData = parentFields'}) (_, Object fields) =
      Object <$> mapM validateField fields
      where
        validateField (_name, value') = do
          (type', currentProp') <- validationData value'
          wrappers' <- fieldTypeWrappers <$> getField
          value'' <- validateInputValue lib' currentProp' wrappers' type' (_name, value')
          return (_name, value'')
          where
            validationData x = do
              fieldTypeName' <- fieldType <$> getField
              let currentProp = prop' ++ [Prop _name fieldTypeName']
              type' <- getInputType fieldTypeName' lib' (typeMismatch x fieldTypeName' currentProp)
              return (type', currentProp)
            getField = lookupField _name parentFields' (UnknownField prop' _name)
    {-- VALIDATE SCALAR --}
    validate [] (EnumKind DataType {typeData = tags', typeName = name'}) (_, value') =
      validateEnum (UnexpectedType prop' name' value') tags' value'
    {-- VALIDATE ENUM --}
    validate [] (ScalarKind DataType {typeName = name', typeData = DataValidator {validateValue = validator'}}) (_, value') =
      case validator' value' of
        Right _            -> return value'
        Left _errorMessage -> Left $ UnexpectedType prop' name' value' -- TODO: for next release add custom error messages
    {-- 3. THROW ERROR: on invalid values --}
    validate wrappers' type' (_, value') = throwError wrappers' type' value'
