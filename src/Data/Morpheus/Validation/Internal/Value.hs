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
import           Data.Morpheus.Types.Internal.AST
                                                ( DataField(..)
                                                , DataTypeContent(..)
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
                                                , Value(..)
                                                )

import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..) )

-- Validate Variable Argument or all Possible input Values
validateInputValue
  :: DataTypeLib
  -> [Prop]
  -> [TypeWrapper]
  -> DataType
  -> (Key, Value)
  -> InputValidation Value
validateInputValue lib prop' rw datatype@DataType { typeContent , typeName } = validate rw typeContent
 where
  throwError :: [TypeWrapper]  -> Value -> InputValidation Value
  throwError wrappers  value =
    Left $ UnexpectedType prop' (renderWrapped datatype wrappers) value Nothing
  -- VALIDATION
  validate :: [TypeWrapper] -> DataTypeContent -> (Key, Value) -> InputValidation Value
  -- Validate Null. value = null ?
  validate wrappers _ (_, Null) | isNullable wrappers = return Null
                                    | otherwise = throwError wrappers  Null
  -- Validate LIST
  validate (TypeMaybe : wrappers) _ value' =
    validateInputValue lib prop' wrappers datatype value'
  validate (TypeList : wrappers) _ (key', List list') =
    List <$> mapM validateElement list'
   where
    validateElement element' =
      validateInputValue lib prop' wrappers datatype (key', element')
  {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
  {-- VALIDATE OBJECT--}
  validate [] (DataInputObject parentFields) (_, Object fields)
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
      getField = lookupField _name parentFields (UnknownField prop' _name)
  -- VALIDATE INPUT UNION
  -- TODO: Validate Union
  validate [] (DataInputUnion _) (_, Object fields) =
    return (Object fields)
  {-- VALIDATE SCALAR --}
  validate [] (DataEnum tags) (_, value)
    = validateEnum (UnexpectedType prop' typeName value Nothing) tags value
  {-- VALIDATE ENUM --}
  validate [] (DataScalar DataValidator { validateValue } ) (_, value')
    = case validateValue value' of
      Right _  -> return value'
      Left  "" -> failure (UnexpectedType prop' typeName value' Nothing)
      Left errorMessage ->
        Left $ UnexpectedType prop' typeName value' (Just errorMessage)
  {-- 3. THROW ERROR: on invalid values --}
  validate wrappers _ (_, value) = throwError wrappers value

validateEnum :: error -> [DataEnumValue] -> Value -> Either error Value
validateEnum gqlError enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = Left gqlError
  where tags = map enumName enumValues
validateEnum gqlError _ _ = Left gqlError

typeMismatch :: Value -> Key -> [Prop] -> InputError
typeMismatch jsType expected' path' =
  UnexpectedType path' expected' jsType Nothing
