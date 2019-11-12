{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInputValue
  , validateEnum
  )
where

import           Data.List                      ( elem )

-- MORPHEUS
import           Data.Morpheus.Error.Input      ( InputError(..)
                                                , InputValidation
                                                , Prop(..)
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( renderWrapped )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataField(..)
                                                , DataTyCon(..)
                                                , DataType(..)
                                                , DataTypeLib(..)
                                                , DataValidator(..)
                                                , Key
                                                , TypeAlias(..)
                                                , TypeWrapper(..)
                                                , DataEnumValue(..)
                                                , isNullable
                                                , lookupField
                                                , lookupInputType
                                                )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value(..) )

-- Validate Variable Argument or all Possible input Values
validateInputValue
  :: DataTypeLib
  -> [Prop]
  -> [TypeWrapper]
  -> DataType
  -> (Key, Value)
  -> InputValidation Value
validateInputValue lib prop' = validate
 where
  throwError :: [TypeWrapper] -> DataType -> Value -> InputValidation Value
  throwError wrappers datatype value =
    Left $ UnexpectedType prop' (renderWrapped datatype wrappers) value Nothing
  -- VALIDATION
  validate :: [TypeWrapper] -> DataType -> (Key, Value) -> InputValidation Value
  -- Validate Null. value = null ?
  validate wrappers tName (_, Null) | isNullable wrappers = return Null
                                    | otherwise = throwError wrappers tName Null
  -- Validate LIST
  validate (TypeMaybe : wrappers) type' value' =
    validateInputValue lib prop' wrappers type' value'
  validate (TypeList : wrappers) type' (key', List list') =
    List <$> mapM validateElement list'
   where
    validateElement element' =
      validateInputValue lib prop' wrappers type' (key', element')
  {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
  {-- VALIDATE OBJECT--}
  validate [] (DataInputObject DataTyCon { typeData = parentFields' }) (_, Object fields)
    = Object <$> mapM validateField fields
   where
    validateField (_name, value) = do
      (type', currentProp') <- validationData value
      wrappers'             <- aliasWrappers . fieldType <$> getField
      value''               <- validateInputValue lib
                                                  currentProp'
                                                  wrappers'
                                                  type'
                                                  (_name, value)
      return (_name, value'')
     where
      validationData x = do
        fieldTypeName' <- aliasTyCon . fieldType <$> getField
        let currentProp = prop' ++ [Prop _name fieldTypeName']
        type' <- lookupInputType fieldTypeName'
                                 lib
                                 (typeMismatch x fieldTypeName' currentProp)
        return (type', currentProp)
      getField = lookupField _name parentFields' (UnknownField prop' _name)
  -- VALIDATE INPUT UNION
  -- TODO: Validate Union
  validate [] (DataInputUnion DataTyCon { typeData }) (_, Object fields) =
    return (Object fields)
  {-- VALIDATE SCALAR --}
  validate [] (DataEnum DataTyCon { typeData = tags, typeName = name' }) (_, value')
    = validateEnum (UnexpectedType prop' name' value' Nothing) tags value'
  {-- VALIDATE ENUM --}
  validate [] (DataScalar DataTyCon { typeName = name', typeData = DataValidator { validateValue = validator' } }) (_, value')
    = case validator' value' of
      Right _  -> return value'
      Left  "" -> Left $ UnexpectedType prop' name' value' Nothing
      Left errorMessage ->
        Left $ UnexpectedType prop' name' value' (Just errorMessage)
  {-- 3. THROW ERROR: on invalid values --}
  validate wrappers datatype (_, value) = throwError wrappers datatype value

validateEnum :: error -> [DataEnumValue] -> Value -> Either error Value
validateEnum gqlError enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = Left gqlError
  where tags = map enumName enumValues
validateEnum gqlError _ _ = Left gqlError

typeMismatch :: Value -> Key -> [Prop] -> InputError
typeMismatch jsType expected' path' =
  UnexpectedType path' expected' jsType Nothing
