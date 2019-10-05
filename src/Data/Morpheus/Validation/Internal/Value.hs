{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInputValue
  , validateEnum
  ) where

import           Data.List                               (elem)

-- MORPHEUS
import           Data.Morpheus.Error.Input               (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.Rendering.RenderGQL       (renderWrapped)
import           Data.Morpheus.Types.Internal.Data       (DataField (..), DataKind (..), DataTyCon (..),
                                                          DataTypeLib (..), DataValidator (..), Key, TypeAlias (..),
                                                          WrapperD (..), isNullable)
import           Data.Morpheus.Types.Internal.Value      (Value (..))
import           Data.Morpheus.Validation.Internal.Utils (getInputType, lookupField)

-- Validate Variable Argument or all Possible input Values
validateInputValue :: DataTypeLib -> [Prop] -> [WrapperD] -> DataKind -> (Key, Value) -> InputValidation Value
validateInputValue lib prop' = validate
  where
    throwError :: [WrapperD] -> DataKind -> Value -> InputValidation Value
    throwError wrappers type' value' = Left $ UnexpectedType prop' (renderWrapped type' wrappers) value' Nothing
    -- VALIDATION
    validate :: [WrapperD] -> DataKind -> (Key, Value) -> InputValidation Value
    -- Validate Null. value = null ?
    validate wrappers tName (_, Null)
      | isNullable wrappers = return Null
      | otherwise = throwError wrappers tName Null
    -- Validate LIST
    validate (MaybeD:wrappers) type' value' = validateInputValue lib prop' wrappers type' value'
    validate (ListD:wrappers) type' (key', List list') = List <$> mapM validateElement list'
      where
        validateElement element' = validateInputValue lib prop' wrappers type' (key', element')
    {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
    {-- VALIDATE OBJECT--}
    validate [] (ObjectKind DataTyCon {typeData = parentFields'}) (_, Object fields) =
      Object <$> mapM validateField fields
      where
        validateField (_name, value) = do
          (type', currentProp') <- validationData value
          wrappers' <- aliasWrappers . fieldType <$> getField
          value'' <- validateInputValue lib currentProp' wrappers' type' (_name, value)
          return (_name, value'')
          where
            validationData x = do
              fieldTypeName' <- aliasTyCon . fieldType <$> getField
              let currentProp = prop' ++ [Prop _name fieldTypeName']
              type' <- getInputType fieldTypeName' lib (typeMismatch x fieldTypeName' currentProp)
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

validateEnum :: error -> [Key] -> Value -> Either error Value
validateEnum error' tags' (Enum enumValue) =
  if enumValue `elem` tags'
    then pure (Enum enumValue)
    else Left error'
validateEnum error' _ _ = Left error'

typeMismatch :: Value -> Key -> [Prop] -> InputError
typeMismatch jsType expected' path' = UnexpectedType path' expected' jsType Nothing
