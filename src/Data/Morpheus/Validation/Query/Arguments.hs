{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  )
where

import           Data.Foldable                  (traverse_)
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
                                                , InputSourceType(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..)
                                                , empty
                                                )
import           Data.Morpheus.Types.Internal.Validator
                                                ( SelectionValidator
                                                , selectKnown
                                                , selectRequired
                                                , selectWithDefaultValue
                                                , askScopePosition
                                                , withScopePosition
                                                , askInputFieldType
                                                , startInput
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInput )

-- only Resolves , doesnot checks the types
resolveObject :: VariableDefinitions VALID -> RawValue -> SelectionValidator ResolvedValue
resolveObject variables = resolve
 where
  resolveEntry :: ObjectEntry RAW -> SelectionValidator (ObjectEntry RESOLVED)
  resolveEntry (ObjectEntry name v) = ObjectEntry name <$> resolve v
  ------------------------------------------------
  resolve :: RawValue -> SelectionValidator ResolvedValue
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
  -> SelectionValidator (Arguments RESOLVED)
resolveArgumentVariables variables
  = traverse resolveVariable
 where
  resolveVariable :: Argument RAW -> SelectionValidator (Argument RESOLVED)
  resolveVariable (Argument key val position) = do 
    constValue <- resolveObject variables val
    pure $ Argument key constValue position

validateArgument
  :: Arguments RESOLVED
  -> ArgumentDefinition
  -> SelectionValidator (Argument VALID)
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
  validateArgumentValue :: Argument RESOLVED -> SelectionValidator (Argument VALID)
  validateArgumentValue arg@Argument { argumentValue = value, .. } =
    withScopePosition argumentPosition $ do
      datatype <- askInputFieldType argumentDef
      argumentValue 
          <- startInput (SourceArgument arg)
            $ validateInput
                typeWrappers 
                datatype 
                (ObjectEntry fieldName value)
      pure Argument { argumentValue , .. }

validateArguments
  :: VariableDefinitions VALID
  -> FieldDefinition
  -> Arguments RAW
  -> SelectionValidator (Arguments VALID)
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
  checkUnknown :: Argument RESOLVED -> SelectionValidator ArgumentDefinition
  checkUnknown = (`selectKnown` fieldDef)