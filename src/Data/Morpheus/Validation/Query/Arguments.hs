{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  )
where

import           Data.Maybe                     ( maybe )
import           Data.Morpheus.Error.Arguments  ( argumentGotInvalidValue
                                                , argumentNameCollision
                                                , undefinedArgument
                                                , unknownArguments
                                                )
import           Data.Morpheus.Error.Input      ( InputValidation
                                                , inputErrorMessage
                                                )
import           Data.Morpheus.Error.Internal   ( internalUnknownTypeMessage )
import           Data.Morpheus.Error.Variable   ( undefinedVariable )
import           Data.Morpheus.Types.Internal.AST
                                                ( ValidVariables
                                                , Variable(..)
                                                , Argument(..)
                                                , RawArgument
                                                , RawArguments
                                                , ValidArgument
                                                , ValidArguments
                                                , Arguments
                                                , Ref(..)
                                                , Position
                                                , DataArgument
                                                , DataField(..)
                                                , Schema
                                                , TypeRef(..)
                                                , isFieldNullable
                                                , lookupInputType
                                                , Value(..)
                                                , Name
                                                , RawValue
                                                , ResolvedValue
                                                , RESOLVED
                                                , VALID
                                                , checkForUnknownKeys
                                                , checkNameCollision
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInputValue )
import           Data.Text                      ( Text )

-- only Resolves , doesnot checks the types
resolveObject :: Name -> ValidVariables -> RawValue -> Validation ResolvedValue
resolveObject operationName variables = resolve
 where
  resolve :: RawValue -> Validation ResolvedValue
  resolve Null         = pure Null
  resolve (Scalar x  ) = pure $ Scalar x
  resolve (Enum   x  ) = pure $ Enum x
  resolve (List   x  ) = List <$> traverse resolve x
  resolve (Object obj) = Object <$> traverse mapSecond obj
    where mapSecond (fName, y) = (fName, ) <$> resolve y
  resolve (VariableValue ref) =
    ResolvedVariable ref <$> variableByRef operationName variables ref
    --  >>= checkTypeEquality ref fieldType
  -- RAW | RESOLVED | Valid 

variableByRef :: Name -> ValidVariables -> Ref -> Validation (Variable VALID)
variableByRef operationName variables Ref { refName, refPosition } = maybe
  variableError
  pure
  (lookup refName variables)
 where
  variableError = failure $ undefinedVariable operationName refPosition refName



resolveArgumentVariables
  :: Name
  -> ValidVariables
  -> DataField
  -> RawArguments
  -> Validation (Arguments RESOLVED)
resolveArgumentVariables operationName variables DataField { fieldName, fieldArgs }
  = mapM resolveVariable
 where
  resolveVariable :: (Text, RawArgument) -> Validation (Text, Argument RESOLVED)
  resolveVariable (key, Argument val position) = case lookup key fieldArgs of
    Nothing -> failure $ unknownArguments fieldName [Ref key position]
    Just _  -> do
      constValue <- resolveObject operationName variables val
      pure (key, Argument constValue position)

validateArgument
  :: Schema
  -> Position
  -> Arguments RESOLVED
  -> (Text, DataArgument)
  -> Validation (Text, ValidArgument)
validateArgument lib fieldPosition requestArgs (key, argType@DataField { fieldType = TypeRef { typeConName, typeWrappers } })
  = case lookup key requestArgs of
    Nothing -> handleNullable
    -- TODO: move it in value validation
   -- Just argument@Argument { argumentOrigin = VARIABLE } ->
   --   pure (key, argument) -- Variables are already checked in Variable Validation
    Just Argument { argumentValue = Null } -> handleNullable
    Just argument -> validateArgumentValue argument
 where
  handleNullable
    | isFieldNullable argType
    = pure
      (key, Argument { argumentValue = Null, argumentPosition = fieldPosition })
    | otherwise
    = failure $ undefinedArgument (Ref key fieldPosition)
  -------------------------------------------------------------------------
  validateArgumentValue :: Argument RESOLVED -> Validation (Text, ValidArgument)
  validateArgumentValue Argument { argumentValue = value, argumentPosition } =
    do
      datatype <- lookupInputType typeConName
                                  lib
                                  (internalUnknownTypeMessage typeConName)
      argumentValue <- handleInputError
        $ validateInputValue lib [] typeWrappers datatype (key, value)
      pure (key, Argument { argumentValue, argumentPosition })
   where
    ---------
    handleInputError :: InputValidation a -> Validation a
    handleInputError (Left err) = failure $ case inputErrorMessage err of
      Left  errors  -> errors
      Right message -> argumentGotInvalidValue key message argumentPosition
    handleInputError (Right x) = pure x

validateArguments
  :: Schema
  -> Text
  -> ValidVariables
  -> (Text, DataField)
  -> Position
  -> RawArguments
  -> Validation ValidArguments
validateArguments typeLib operatorName variables (key, field@DataField { fieldArgs }) pos rawArgs
  = do
    args     <- resolveArgumentVariables operatorName variables field rawArgs
    dataArgs <- checkForUnknownArguments args
    mapM (validateArgument typeLib pos args) dataArgs
 where
  checkForUnknownArguments
    :: Arguments RESOLVED -> Validation [(Text, DataField)]
  checkForUnknownArguments args =
    checkForUnknownKeys enhancedKeys fieldKeys argError
      >> checkNameCollision enhancedKeys argumentNameCollision
      >> pure fieldArgs
   where
    argError     = unknownArguments key
    enhancedKeys = map argToKey args
    argToKey :: (Name, Argument RESOLVED) -> Ref
    argToKey (key', Argument { argumentPosition }) = Ref key' argumentPosition
    fieldKeys = map fst fieldArgs
