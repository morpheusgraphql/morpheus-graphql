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
                                                , ValueOrigin(..)
                                                , RawArgument
                                                , RawArguments
                                                , ValidArgument
                                                , ValidArguments
                                                , Ref(..)
                                                , Position
                                                , DataArgument
                                                , DataField(..)
                                                , DataTypeLib
                                                , TypeAlias(..)
                                                , isFieldNullable
                                                , lookupInputType
                                                , Value(..)
                                                , Name
                                                , RawValue
                                                , ValidValue
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Validation.Internal.Utils
                                                ( checkForUnknownKeys
                                                , checkNameCollision
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInputValue )
import           Data.Text                      ( Text )

-- only Resolves , doesnot checks the types
resolveObject :: Name -> ValidVariables -> RawValue -> Validation ValidValue
resolveObject operationName variables = resolve
 where
  resolve :: RawValue -> Validation ValidValue
  resolve Null         = pure Null
  resolve (Scalar x  ) = pure $ Scalar x
  resolve (Enum   x  ) = pure $ Enum x
  resolve (List   x  ) = List <$> traverse resolve x
  resolve (Object obj) = Object <$> traverse mapSecond obj
    where mapSecond (fName, y) = (fName, ) <$> resolve y
  resolve (VariableValue ref) =
    ResolvedValue ref
      .   variableValue
      <$> variableByRef operationName variables ref
    --  >>= checkTypeEquality ref fieldType
  -- RAW | RESOLVED | Valid 

variableByRef
  :: Name -> ValidVariables -> Ref -> Validation (Variable ValidValue)
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
  -> Validation ValidArguments
resolveArgumentVariables operationName variables DataField { fieldName, fieldArgs }
  = mapM resolveVariable
 where
  resolveVariable :: (Text, RawArgument) -> Validation (Text, ValidArgument)
  resolveVariable (key, Argument val origin position) =
    case lookup key fieldArgs of
      Nothing -> failure $ unknownArguments fieldName [Ref key position]
      Just _  -> do
        constValue <- resolveObject operationName variables val
        pure (key, Argument constValue origin position)

validateArgument
  :: DataTypeLib
  -> Position
  -> ValidArguments
  -> (Text, DataArgument)
  -> Validation (Text, ValidArgument)
validateArgument lib fieldPosition requestArgs (key, argType@DataField { fieldType = TypeAlias { aliasTyCon, aliasWrappers } })
  = case lookup key requestArgs of
    Nothing -> handleNullable
    Just argument@Argument { argumentOrigin = VARIABLE } ->
      pure (key, argument) -- Variables are already checked in Variable Validation
    Just Argument { argumentValue = Null } -> handleNullable
    Just argument                          -> validateArgumentValue argument
 where
  handleNullable
    | isFieldNullable argType = pure
      ( key
      , Argument { argumentValue    = Null
                 , argumentOrigin   = INLINE
                 , argumentPosition = fieldPosition
                 }
      )
    | otherwise = failure $ undefinedArgument (Ref key fieldPosition)
  -------------------------------------------------------------------------
  validateArgumentValue :: ValidArgument -> Validation (Text, ValidArgument)
  validateArgumentValue arg@Argument { argumentValue = value, argumentPosition }
    = lookupInputType aliasTyCon lib (internalUnknownTypeMessage aliasTyCon)
      >>= checkType
      >>  pure (key, arg)
   where
    checkType type' = handleInputError
      (validateInputValue lib [] aliasWrappers type' (key, value))
    ---------
    handleInputError :: InputValidation a -> Validation ()
    handleInputError (Left err) = failure
      $ argumentGotInvalidValue key (inputErrorMessage err) argumentPosition
    handleInputError _ = pure ()

validateArguments
  :: DataTypeLib
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
  checkForUnknownArguments :: ValidArguments -> Validation [(Text, DataField)]
  checkForUnknownArguments args =
    checkForUnknownKeys enhancedKeys fieldKeys argError
      >> checkNameCollision enhancedKeys argumentNameCollision
      >> pure fieldArgs
   where
    argError     = unknownArguments key
    enhancedKeys = map argToKey args
    argToKey :: (Name, ValidArgument) -> Ref
    argToKey (key', Argument { argumentPosition }) = Ref key' argumentPosition
    fieldKeys = map fst fieldArgs
