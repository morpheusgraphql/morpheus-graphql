{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  )
where

import           Data.Morpheus.Error.Arguments  ( argumentGotInvalidValue
                                                , argumentNameCollision
                                                , undefinedArgument
                                                , unknownArguments
                                                )
import           Data.Morpheus.Error.Input      ( InputValidation
                                                , inputErrorMessage
                                                )
import           Data.Morpheus.Error.Internal   ( internalUnknownTypeMessage )
import           Data.Morpheus.Error.Variable   ( incompatibleVariableType
                                                , undefinedVariable
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) )
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
                                                , isWeaker
                                                , lookupInputType
                                                , Value(..)
                                                , VariableValue(..)
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



resolveObject :: RawValue -> ValidValue
resolveObject (ConstantValue x) = ConstantValue x
resolveObject (VariableObject obj) = ConstantValue $ Object $ map mapSecond obj
  where
     mapSecond (x,y) = (x, constantValue $ resolveObject y)
resolveObject (VariableList x) = ConstantValue $ List $ map (constantValue . resolveObject ) x
-- TODO: Resolve variables
resolveObject (VariableValue x) = ConstantValue Null




resolveArgumentVariables
  :: Text
  -> ValidVariables
  -> DataField
  -> RawArguments
  -> Validation ValidArguments
resolveArgumentVariables operatorName variables DataField { fieldName, fieldArgs }
  = mapM resolveVariable
 where

  resolveVariable :: (Text, RawArgument) -> Validation (Text, ValidArgument)
  resolveVariable (key, Argument val origin pos) =
    pure (key, Argument (resolveObject val) origin pos)
  resolveVariable (key, VariableRef Ref { refName, refPosition }) =
    (key, ) . toArgument . ConstantValue <$> lookupVar
   where
    toArgument argumentValue = Argument { argumentValue
                                        , argumentOrigin   = VARIABLE
                                        , argumentPosition = refPosition
                                        }
    lookupVar = case lookup refName variables of
      Nothing -> failure $ undefinedVariable operatorName refPosition refName
      Just Variable { variableValue, variableType, variableTypeWrappers } ->
        case lookup key fieldArgs of
          Nothing -> failure $ unknownArguments fieldName [Ref key refPosition]
          Just DataField { fieldType = fieldT@TypeAlias { aliasTyCon, aliasWrappers } }
            -> if variableType == aliasTyCon && not
                 (isWeaker variableTypeWrappers aliasWrappers)
              then return variableValue
              else failure $ incompatibleVariableType refName
                                                      varSignature
                                                      fieldSignature
                                                      refPosition
           where
            varSignature   = renderWrapped variableType variableTypeWrappers
            fieldSignature = render fieldT

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
    Just Argument { argumentValue = ConstantValue Null } -> handleNullable
    Just argument -> validateArgumentValue argument
 where
  handleNullable
    | isFieldNullable argType = pure
      ( key
      , Argument { argumentValue    = ConstantValue Null
                 , argumentOrigin   = INLINE
                 , argumentPosition = fieldPosition
                 }
      )
    | otherwise = failure $ undefinedArgument (Ref key fieldPosition)
  -------------------------------------------------------------------------
  validateArgumentValue :: ValidArgument -> Validation (Text, ValidArgument)
  validateArgumentValue arg@Argument { argumentValue = ConstantValue value, argumentPosition }
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
