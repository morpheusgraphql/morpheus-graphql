{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  )
where

import           Data.Foldable                  (traverse_)
import           Data.Morpheus.Error.Arguments  ( argumentGotInvalidValue
                                                , undefinedArgument
                                                )
import           Data.Morpheus.Error.Internal   ( internalUnknownTypeMessage )
import           Data.Morpheus.Types.Internal.AST
                                                ( VariableDefinitions
                                                , Argument(..)
                                                , ArgumentsDefinition(..)
                                                , Arguments
                                                , Ref(..)
                                                , Position
                                                , ArgumentDefinition
                                                , FieldDefinition(..)
                                                , TypeRef(..)
                                                , Value(..)
                                                , RawValue
                                                , ResolvedValue
                                                , RESOLVED
                                                , VALID
                                                , isFieldNullable
                                                , ObjectEntry(..)
                                                , RAW
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..)
                                                , selectOr
                                                , empty
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validator
                                                ( Validator
                                                , selectKnown
                                                , selectRequired
                                                , lookupInputType
                                                , selectWithDefaultValue
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInput )

-- only Resolves , doesnot checks the types
resolveObject :: VariableDefinitions VALID -> RawValue -> Validator ResolvedValue
resolveObject variables = resolve
 where
  resolveEntry :: ObjectEntry RAW -> Validator (ObjectEntry RESOLVED)
  resolveEntry (ObjectEntry name v) = ObjectEntry name <$> resolve v
  ------------------------------------------------
  resolve :: RawValue -> Validator ResolvedValue
  resolve Null         = pure Null
  resolve (Scalar x  ) = pure $ Scalar x
  resolve (Enum   x  ) = pure $ Enum x
  resolve (List   x  ) = List <$> traverse resolve x
  resolve (Object obj) = Object <$> traverse resolveEntry obj
  resolve (VariableValue ref) =
    ResolvedVariable ref <$> selectRequired ref variables 

resolveArgumentVariables
  :: VariableDefinitions VALID
  -> Arguments RAW
  -> Validator (Arguments RESOLVED)
resolveArgumentVariables variables
  = traverse resolveVariable
 where
  resolveVariable :: Argument RAW -> Validator (Argument RESOLVED)
  resolveVariable (Argument key val position) = do 
    constValue <- resolveObject variables val
    pure $ Argument key constValue position

validateArgument
  :: Position
  -> Arguments RESOLVED
  -> ArgumentDefinition
  -> Validator (Argument VALID)
validateArgument fieldPosition requestArgs argType@FieldDefinition { fieldName, fieldType = TypeRef { typeConName, typeWrappers } }
  = selectWithDefaultValue
      (Argument { argumentName = fieldName, argumentValue = Null, argumentPosition = fieldPosition })
      argType
      requestArgs 
    >>= handleArgument 
    -- >>= validateArgumentValue
  -- = selectOr 
  --   handleNullable 
  --   handleArgument 
  --   fieldName 
  --   requestArgs 
 where
    -- TODO: move it in value validation
   -- Just argument@Argument { argumentOrigin = VARIABLE } ->
   --   pure (key, argument) -- Variables are already checked in Variable Validation
  handleArgument Argument { argumentValue = Null } = handleNullable
  handleArgument argument = validateArgumentValue argument
  handleNullable
    | isFieldNullable argType
    = pure Argument { argumentName = fieldName, argumentValue = Null, argumentPosition = fieldPosition }
    | otherwise
    = failure $ undefinedArgument (Ref fieldName fieldPosition)
  -------------------------------------------------------------------------
  validateArgumentValue :: Argument RESOLVED -> Validator (Argument VALID)
  validateArgumentValue Argument { argumentValue = value, .. } =
    do
      datatype <- lookupInputType typeConName
                          (internalUnknownTypeMessage typeConName)
      argumentValue <- validateInput
                          (argumentGotInvalidValue argumentName,argumentPosition)
                          typeWrappers 
                          datatype 
                          (ObjectEntry fieldName value)
      pure Argument { argumentValue , .. }

validateArguments
  :: VariableDefinitions VALID
  -> FieldDefinition
  -> Position
  -> Arguments RAW
  -> Validator (Arguments VALID)
validateArguments
    variables 
    fieldDef@FieldDefinition {  fieldArgs }
    pos 
    rawArgs
  = do
    args <- resolveArgumentVariables variables rawArgs
    traverse_ checkUnknown (toList args)
    traverse (validateArgument pos args) fArgs
 where
  fArgs = case fieldArgs of 
    (ArgumentsDefinition _ argsD) -> argsD
    NoArguments -> empty
  -------------------------------------------------
  checkUnknown :: Argument RESOLVED -> Validator ArgumentDefinition
  checkUnknown = (`selectKnown` fieldDef)