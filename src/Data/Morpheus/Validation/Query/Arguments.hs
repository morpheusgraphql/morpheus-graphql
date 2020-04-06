{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  )
where

import           Data.Foldable                  (traverse_)
import           Data.Morpheus.Error.Input      ( argumentGotInvalidValue )
import           Data.Morpheus.Types.Internal.AST
                                                ( VariableDefinitions
                                                , Argument(..)
                                                , ArgumentsDefinition(..)
                                                , Arguments
                                                , ArgumentDefinition
                                                , FieldDefinition(..)
                                                , TypeRef(..)
                                                , Value(..)
                                                , RawValue
                                                , ResolvedValue
                                                , RESOLVED
                                                , VALID
                                                , ObjectEntry(..)
                                                , RAW
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..)
                                                , empty
                                                )
import           Data.Morpheus.Types.Internal.Validator
                                                ( Validator
                                                , selectKnown
                                                , selectRequired
                                                , lookupInputType
                                                , selectWithDefaultValue
                                                , askScopePosition
                                                , withScopePosition
                                                , askInputFieldType
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
  :: Arguments RESOLVED
  -> ArgumentDefinition
  -> Validator (Argument VALID)
validateArgument 
    requestArgs 
    argumentDef@FieldDefinition 
      { fieldName 
      , fieldType = TypeRef { typeWrappers } 
      }
  = do 
      argumentPosition <- askScopePosition
      argument <- selectWithDefaultValue
          Argument { argumentName = fieldName, argumentValue = Null, argumentPosition }
          argumentDef
          requestArgs 
      validateArgumentValue argument
 where
  -------------------------------------------------------------------------
  validateArgumentValue :: Argument RESOLVED -> Validator (Argument VALID)
  validateArgumentValue Argument { argumentValue = value, .. } =
    withScopePosition argumentPosition $ do
      datatype <- askInputFieldType argumentDef
      argumentValue <- validateInput
                          (argumentGotInvalidValue argumentName)
                          typeWrappers 
                          datatype 
                          (ObjectEntry fieldName value)
      pure Argument { argumentValue , .. }

validateArguments
  :: VariableDefinitions VALID
  -> FieldDefinition
  -> Arguments RAW
  -> Validator (Arguments VALID)
validateArguments
    variables 
    fieldDef@FieldDefinition {  fieldArgs }
    rawArgs
  = do
    args <- resolveArgumentVariables variables rawArgs
    traverse_ checkUnknown (toList args)
    traverse (validateArgument args) argsDef
 where
  argsDef = case fieldArgs of 
    (ArgumentsDefinition _ argsD) -> argsD
    NoArguments -> empty
  -------------------------------------------------
  checkUnknown :: Argument RESOLVED -> Validator ArgumentDefinition
  checkUnknown = (`selectKnown` fieldDef)