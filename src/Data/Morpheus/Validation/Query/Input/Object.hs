{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Query.Input.Object
  ( validateInputValue
  ) where

import           Data.Morpheus.Error.Input                 (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.Types.Internal.Data         (DataField (..), DataKind (..), DataTyCon (..),
                                                            DataTypeLib (..), DataTypeWrapper (..), DataValidator (..),
                                                            showFullAstType)
import           Data.Morpheus.Types.Internal.Value        (Value (..))
import           Data.Morpheus.Validation.Internal.Utils   (getInputType, lookupField)
import           Data.Morpheus.Validation.Query.Input.Enum (validateEnum)
import           Data.Text                                 (Text)

typeMismatch :: Value -> Text -> [Prop] -> InputError
typeMismatch jsType expected' path' = UnexpectedType path' expected' jsType Nothing

-- Validate Variable Argument or all Possible input Values
validateInputValue :: DataTypeLib -> [Prop] -> [DataTypeWrapper] -> DataKind -> (Text, Value) -> InputValidation Value
validateInputValue lib' prop' = validate
  where
    throwError :: [DataTypeWrapper] -> DataKind -> Value -> InputValidation Value
    throwError wrappers' type' value' = Left $ UnexpectedType prop' (showFullAstType wrappers' type') value' Nothing
    {-- VALIDATION --}
    {-- 1. VALIDATE WRAPPERS -}
    validate :: [DataTypeWrapper] -> DataKind -> (Text, Value) -> InputValidation Value
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
    validate [] (ObjectKind DataTyCon {typeData = parentFields'}) (_, Object fields) =
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
    -- VALIDATE INPUT UNION
    -- TODO: Validate Union
    validate [] (UnionKind DataTyCon {typeData}) (_, Object fields) = return (Object fields)
    {-- VALIDATE SCALAR --}
    validate [] (EnumKind DataTyCon {typeData = tags', typeName = name'}) (_, value') =
      validateEnum (UnexpectedType prop' name' value' Nothing) tags' value'
    {-- VALIDATE ENUM --}
    validate [] (ScalarKind DataTyCon {typeName = name', typeData = DataValidator {validateValue = validator'}}) (_, value') =
      case validator' value' of
        Right _           -> return value'
        Left ""           -> Left $ UnexpectedType prop' name' value' Nothing
        Left errorMessage -> Left $ UnexpectedType prop' name' value' (Just errorMessage)
    {-- 3. THROW ERROR: on invalid values --}
    validate wrappers' type' (_, value') = throwError wrappers' type' value'
