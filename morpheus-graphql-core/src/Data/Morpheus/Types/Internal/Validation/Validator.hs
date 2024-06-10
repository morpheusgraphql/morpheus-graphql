{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation.Validator
  ( Validator (..),
    SelectionValidator,
    InputValidator,
    BaseValidator,
    runValidator,
    Constraint (..),
    setSelection,
    inField,
    inputMessagePrefix,
    InputSource (..),
    InputContext (..),
    OperationContext (..),
    renderInputPrefix,
    Prop (..),
    --  Resolution,
    ScopeKind (..),
    inputValueSource,
    Scope (..),
    setDirective,
    startInput,
    withContext,
    renderField,
    -- asks,
    asksScope,
    askVariables,
    askFragments,
    ValidatorContext (..),
    FragmentValidator,
    askTypeDefinitions,
    withScope,
    setPosition,
  )
where

import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Reader (asks)
import Data.Morpheus.Ext.Result
  ( GQLResult,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (..),
    FieldName,
    Fragments,
    IMPLEMENTABLE,
    IN,
    RAW,
    Schema,
    Stage,
    TypeCategory,
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    intercalate,
    typeDefinitions,
    unpackName,
  )
import Data.Morpheus.Types.Internal.AST.Error
import Data.Morpheus.Types.Internal.Config (Config (..))
import Data.Morpheus.Types.Internal.Validation.Scope
  ( Scope (..),
    ScopeKind (..),
    renderScope,
    renderSection,
    setDirective,
    setPosition,
    setSelection,
  )
import Relude hiding
  ( Constraint,
    asks,
    get,
    intercalate,
  )

data Prop = Prop
  { propName :: FieldName,
    propTypeName :: TypeName
  }
  deriving (Show)

type Path = [Prop]

renderPath :: Path -> GQLError
renderPath [] = ""
renderPath path = "in field " <> msg (intercalate "." $ fmap propName path) <> ": "

renderInputPrefix :: InputContext c -> GQLError
renderInputPrefix InputContext {inputPath, inputSource} =
  renderSource inputSource <> renderPath inputPath

renderSource :: InputSource -> GQLError
renderSource (SourceArgument argumentName) =
  "Argument " <> msg argumentName <> " got invalid value. "
renderSource (SourceVariable Variable {variableName} _) =
  "Variable " <> msg ("$" <> variableName) <> " got invalid value. "
renderSource SourceInputField {sourceTypeName, sourceFieldName, sourceArgumentName} =
  "Field " <> renderField sourceTypeName sourceFieldName sourceArgumentName <> " got invalid default value. "

renderField :: TypeName -> FieldName -> Maybe FieldName -> GQLError
renderField tName fName arg =
  msg (unpackName tName <> "." <> unpackName fName <> renderArg arg :: Text)
  where
    renderArg (Just argName) = "(" <> unpackName argName <> ":)"
    renderArg Nothing = ""

data OperationContext (s1 :: Stage) (s2 :: Stage) = OperationContext
  { fragments :: Fragments s2,
    variables :: VariableDefinitions s1,
    operationName :: Maybe FieldName
  }
  deriving (Show)

data InputContext ctx = InputContext
  { inputSource :: InputSource,
    inputPath :: [Prop],
    sourceContext :: ctx
  }
  deriving (Show)

data InputSource
  = SourceArgument FieldName
  | SourceVariable
      { sourceVariable :: Variable RAW,
        isDefaultValue :: Bool
      }
  | SourceInputField
      { sourceTypeName :: TypeName,
        sourceFieldName :: FieldName,
        sourceArgumentName :: Maybe FieldName
      }
  deriving (Show)

data Constraint (a :: TypeCategory) where
  IMPLEMENTABLE :: Constraint IMPLEMENTABLE
  INPUT :: Constraint IN

inField :: FieldDefinition IN s -> InputValidator s c a -> InputValidator s c a
inField
  FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeConName}
    } = withContext update
    where
      update
        InputContext
          { inputPath = old,
            ..
          } =
          InputContext
            { inputPath = old <> [Prop fieldName typeConName],
              ..
            }

inputValueSource :: (MonadReader (ValidatorContext s (InputContext c)) m) => m InputSource
inputValueSource = asksLocal inputSource

asksScope :: (MonadReader (ValidatorContext s ctx) m) => (Scope -> a) -> m a
asksScope f = asks (f . scope)

askTypeDefinitions ::
  (MonadReader (ValidatorContext s ctx) m) =>
  m (HashMap TypeName (TypeDefinition ANY s))
askTypeDefinitions = asks (typeDefinitions . schema)

askVariables :: (MonadReader (ValidatorContext s1 (OperationContext s2 s3)) m) => m (VariableDefinitions s2)
askVariables = asksLocal variables

askFragments :: (MonadReader (ValidatorContext s1 (OperationContext s2 s3)) m) => m (Fragments s3)
askFragments = asksLocal fragments

runValidator :: Validator s ctx a -> Config -> Schema s -> Scope -> ctx -> GQLResult a
runValidator (Validator x) config schema scope localContext =
  runReaderT x ValidatorContext {..}

withContext :: (c' -> c) -> Validator s c a -> Validator s c' a
withContext f = Validator . withReaderT (fmap f) . _runValidator

inputMessagePrefix :: InputValidator s ctx GQLError
inputMessagePrefix =
  renderInputPrefix
    . localContext
    <$> Validator ask

startInput :: InputSource -> InputValidator s ctx a -> Validator s ctx a
startInput inputSource = withContext update
  where
    update sourceContext =
      InputContext
        { inputSource,
          inputPath = [],
          sourceContext
        }

data ValidatorContext (s :: Stage) (ctx :: Type) = ValidatorContext
  { scope :: Scope,
    schema :: Schema s,
    localContext :: ctx,
    config :: Config
  }
  deriving
    ( Show,
      Functor
    )

newtype Validator s ctx a = Validator
  { _runValidator ::
      ReaderT
        (ValidatorContext s ctx)
        GQLResult
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (ValidatorContext s ctx)
    )

data ValidationTarget
  = Base
  | Fragments
  | Selections

type family ValidationStage (s :: ValidationTarget) where
  ValidationStage 'Base = OperationContext RAW RAW
  ValidationStage 'Fragments = OperationContext VALID RAW
  ValidationStage 'Selections = OperationContext VALID VALID

type ValidatorM (s :: ValidationTarget) = Validator VALID (ValidationStage s)

type BaseValidator = ValidatorM 'Base

type FragmentValidator (s :: Stage) = Validator VALID (OperationContext VALID s)

type SelectionValidator = ValidatorM 'Selections

type InputValidator s ctx = Validator s (InputContext ctx)

withScope ::
  (MonadReader (ValidatorContext s c) m) =>
  (Scope -> Scope) ->
  m b ->
  m b
withScope f = local (\ValidatorContext {..} -> ValidatorContext {scope = f scope, ..})

asksLocal :: (MonadReader (ValidatorContext s c) m) => (c -> a) -> m a
asksLocal f = asks (f . localContext)

instance MonadError GQLError (Validator s ctx) where
  throwError err = Validator $ do
    ctx <- ask
    throwError (fromValidationError ctx err)
  catchError (Validator x) f = Validator (catchError x (_runValidator . f))

fromValidationError :: ValidatorContext s ctx -> GQLError -> GQLError
fromValidationError
  context@ValidatorContext
    { config,
      scope = Scope {position, path}
    }
  err
    | isInternal err || debug config =
        ( err
            <> renderContext context
            `atPositions` position
        )
          `withPath` path
    | otherwise = err

renderContext :: ValidatorContext s ctx -> GQLError
renderContext
  ValidatorContext
    { schema,
      scope
    } =
    renderScope scope
      <> renderSection "SchemaDefinition" schema
