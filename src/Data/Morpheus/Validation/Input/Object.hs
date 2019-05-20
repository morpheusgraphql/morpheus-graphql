{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Input.Object
  ( validateInputValue
  ) where

import           Data.Morpheus.Error.Input            (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.Types.Internal.AST     (ASTField (..), ASTInputField, ASTInputType, ASTKind (..),
                                                       ASTType (..), ASTTypeLib (..), showFullAstType)
import           Data.Morpheus.Types.Internal.Value   (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Query.Operator   (TypeWrapper (..))
import           Data.Morpheus.Validation.Input.Enum  (validateEnum)
import           Data.Morpheus.Validation.Utils.Utils (getInputType, lookupField)
import           Data.Text                            (Text)

typeMismatch :: Value -> Text -> [Prop] -> InputError
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
validateInputValue :: ASTTypeLib -> [Prop] -> [TypeWrapper] -> ASTInputType -> (Text, Value) -> InputValidation Value
validateInputValue lib' prop' = validate
  where
    throwError :: [TypeWrapper] -> ASTInputType -> Value -> InputValidation Value
    throwError wrappers' type' value' = Left $ UnexpectedType prop' (showFullAstType wrappers' type') value'
    {-- VALIDATION --}
    {-- 1. VALIDATE WRAPPERS -}
    validate :: [TypeWrapper] -> ASTInputType -> (Text, Value) -> InputValidation Value
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
    validate [] (ObjectKind ASTType {typeData = parentFields'}) (_, Object fields) =
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
    validate [] (EnumKind ASTType {typeData = tags', typeName = name'}) (_, value') =
      validateEnum (UnexpectedType prop' name' value') tags' value'
    {-- VALIDATE ENUM --}
    validate [] (ScalarKind ASTType {typeName = name'}) (_, Scalar value') =
      Scalar <$> validateScalarTypes name' value' prop'
    {-- 3. THROW ERROR: on invalid values --}
    validate wrappers' type' (_, value') = throwError wrappers' type' value'
