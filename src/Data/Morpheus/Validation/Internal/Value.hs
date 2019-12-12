{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInputValue
  , validateEnum
  )
where

import           Data.List                      ( elem )

-- MORPHEUS
import           Data.Morpheus.Error.Variable   ( incompatibleVariableType )
import           Data.Morpheus.Error.Input      ( InputError(..)
                                                , InputValidation
                                                , Prop(..)
                                                )
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
                                                , ValidValue
                                                , Variable(..)
                                                , Ref(..)
                                                , isWeaker
                                                , DataScalar
                                                , Message
                                                , Name
                                                )

import           Data.Morpheus.Types.Internal.Resolving
                                                ( Failure(..) )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) )

checkTypeEquality
  :: (Name, [TypeWrapper])
  -> Ref
  -> Variable ValidValue
  -> InputValidation ValidValue
checkTypeEquality (aliasTyCon, aliasWrappers) Ref { refName, refPosition } Variable { variableValue, variableType, variableTypeWrappers }
  | variableType == aliasTyCon && not
    (isWeaker variableTypeWrappers aliasWrappers)
  = pure variableValue
  | otherwise
  = failure $ GlobalInputError $ incompatibleVariableType refName
                                                          varSignature
                                                          fieldSignature
                                                          refPosition
 where
  varSignature = renderWrapped variableType variableTypeWrappers
  fieldSignature =
    render TypeAlias { aliasTyCon, aliasWrappers, aliasArgs = Nothing }



-- Validate Variable Argument or all Possible input Values
validateInputValue
  :: DataTypeLib
  -> [Prop]
  -> [TypeWrapper]
  -> DataType
  -> (Key, ValidValue)
  -> InputValidation ValidValue
validateInputValue lib props rw datatype@DataType { typeContent, typeName } =
  validateWrapped rw typeContent
 where
  throwError :: [TypeWrapper] -> ValidValue -> InputValidation ValidValue
  throwError wrappers value =
    Left $ UnexpectedType props (renderWrapped datatype wrappers) value Nothing
  -- VALIDATION
  validateWrapped
    :: [TypeWrapper]
    -> DataTypeContent
    -> (Key, ValidValue)
    -> InputValidation ValidValue
  -- Validate Null. value = null ?
  validateWrapped wrappers dt (name, ResolvedVariable ref variable) =
    checkTypeEquality (typeName, wrappers) ref variable
      >>= validateWrapped wrappers dt
      .   (name, )
  validateWrapped wrappers _ (_, Null) | isNullable wrappers = return Null
                                       | otherwise = throwError wrappers Null
  -- Validate LIST
  validateWrapped (TypeMaybe : wrappers) _ value =
    validateInputValue lib props wrappers datatype value
  validateWrapped (TypeList : wrappers) _ (key, List list) =
    List <$> mapM validateElement list
   where
    validateElement element =
      validateInputValue lib props wrappers datatype (key, element)
  {-- 2. VALIDATE TYPES, all wrappers are already Processed --}
  {-- VALIDATE OBJECT--}
  validateWrapped [] dt v = validate dt v
   where
    validate
      :: DataTypeContent -> (Key, ValidValue) -> InputValidation ValidValue
    validate (DataInputObject parentFields) (_, Object fields) =
      Object <$> mapM validateField fields
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
          let currentProp = props ++ [Prop _name fieldTypeName']
          type' <- lookupInputType fieldTypeName'
                                   lib
                                   (typeMismatch x fieldTypeName' currentProp)
          return (type', currentProp)
        getField = lookupField _name parentFields (UnknownField props _name)
    -- VALIDATE INPUT UNION
    -- TODO: Validate Union
    validate (DataInputUnion _) (_, Object fields) = return (Object fields)
    {-- VALIDATE ENUM --}
    validate (DataEnum tags) (_, value) =
      validateEnum (UnexpectedType props typeName value Nothing) tags value
    {-- VALIDATE SCALAR --}
    validate (DataScalar dataScalar) (_, value) =
      validateScalar dataScalar value (UnexpectedType props typeName)
    validate _ (_, value) = throwError [] value
    {-- 3. THROW ERROR: on invalid values --}
  validateWrapped wrappers _ (_, value) = throwError wrappers value


validateScalar
  :: DataScalar
  -> ValidValue
  -> (ValidValue -> Maybe Message -> error)
  -> Either error ValidValue
validateScalar DataValidator { validateValue } value err =
  case validateValue value of
    Right _            -> return value
    Left  ""           -> failure (err value Nothing)
    Left  errorMessage -> failure $ err value (Just errorMessage)

validateEnum
  :: error -> [DataEnumValue] -> ValidValue -> Either error ValidValue
validateEnum gqlError enumValues (Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise             = Left gqlError
  where tags = map enumName enumValues
validateEnum gqlError _ _ = Left gqlError

typeMismatch :: ValidValue -> Key -> [Prop] -> InputError
typeMismatch jsType expected' path' =
  UnexpectedType path' expected' jsType Nothing
