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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation.Validator
  ( Validator (..),
    SelectionValidator,
    InputValidator,
    BaseValidator,
    runValidator,
    Constraint (..),
    withScope,
    withScopeType,
    withPosition,
    withInputScope,
    inputMessagePrefix,
    InputSource (..),
    InputContext (..),
    OperationContext (..),
    CurrentSelection (..),
    renderInputPrefix,
    Target (..),
    Prop (..),
    Resolution,
    ScopeKind (..),
    inputValueSource,
    Scope (..),
    withDirective,
    startInput,
    GetWith (..),
    SetWith (..),
    MonadContext (..),
    withContext,
    renderField,
    asks,
    asksScope,
    askSchema,
    askVariables,
    askFragments,
    DirectiveValidator,
    ValidatorContext (..),
  )
where

-- MORPHEUS

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    withReaderT,
  )
import Data.Functor ((<$>), Functor (..))
import Data.Maybe (Maybe (..))
import Data.Morpheus.Error.Utils (renderErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Rendering.RenderGQL (RenderGQL (..))
import Data.Morpheus.Types.Internal.AST
  ( Directive (..),
    FieldName (..),
    FieldsDefinition,
    Fragments,
    GQLErrors,
    IN,
    InternalError,
    Message,
    OUT,
    Position,
    RAW,
    Ref (..),
    Schema,
    Stage,
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    intercalateName,
    kindOf,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup
  ( (<>),
    stimes,
  )
import Prelude
  ( ($),
    (.),
    Bool,
    Int,
    Show,
    id,
  )

data Prop = Prop
  { propName :: FieldName,
    propTypeName :: TypeName
  }
  deriving (Show)

type Path = [Prop]

renderPath :: Path -> Message
renderPath [] = ""
renderPath path = "in field " <> msg (intercalateName "." $ fmap propName path) <> ": "

renderInputPrefix :: InputContext c -> Message
renderInputPrefix InputContext {inputPath, inputSource} =
  renderSource inputSource <> renderPath inputPath

renderSource :: InputSource -> Message
renderSource (SourceArgument argumentName) =
  "Argument " <> msg argumentName <> " got invalid value. "
renderSource (SourceVariable Variable {variableName} _) =
  "Variable " <> msg ("$" <> variableName) <> " got invalid value. "
renderSource SourceInputField {sourceTypeName, sourceFieldName, sourceArgumentName} =
  "Field " <> renderField sourceTypeName sourceFieldName sourceArgumentName <> " got invalid default value. "

renderField :: TypeName -> FieldName -> Maybe FieldName -> Message
renderField (TypeName tname) (FieldName fname) arg =
  msg (tname <> "." <> fname <> renderArg arg)
  where
    renderArg (Just (FieldName argName)) = "(" <> argName <> ":)"
    renderArg Nothing = ""

data ScopeKind
  = DIRECTIVE
  | SELECTION
  | TYPE
  deriving (Show)

data OperationContext vars = OperationContext
  { schema :: Schema VALID,
    fragments :: Fragments,
    selection :: CurrentSelection,
    variables :: vars
  }
  deriving (Show)

newtype CurrentSelection = CurrentSelection
  { operationName :: Maybe FieldName
  }
  deriving (Show)

data Scope = Scope
  { position :: Maybe Position,
    currentTypeName :: TypeName,
    currentTypeKind :: TypeKind,
    fieldname :: FieldName,
    kind :: ScopeKind
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

data Target
  = TARGET_OBJECT
  | TARGET_INPUT

data Constraint (a :: Target) where
  OBJECT :: Constraint 'TARGET_OBJECT
  INPUT :: Constraint 'TARGET_INPUT

--  UNION  :: Constraint 'TARGET_UNION

type family Resolution (s :: Stage) (a :: Target)

type instance Resolution s 'TARGET_OBJECT = (TypeName, FieldsDefinition OUT s)

type instance Resolution s 'TARGET_INPUT = TypeDefinition IN s

withInputScope :: Prop -> InputValidator c a -> InputValidator c a
withInputScope prop = withContext update
  where
    update
      InputContext
        { inputPath = old,
          ..
        } =
        InputContext
          { inputPath = old <> [prop],
            ..
          }

inputValueSource ::
  forall m c.
  ( GetWith c InputSource,
    MonadContext m c
  ) =>
  m c InputSource
inputValueSource = get

asks ::
  ( MonadContext m c,
    GetWith c t
  ) =>
  (t -> a) ->
  m c a
asks f = f <$> get

asksScope ::
  ( MonadContext m c
  ) =>
  (Scope -> a) ->
  m c a
asksScope f = f <$> getGlobalContext scope

setSelectionName ::
  (MonadContext m c) =>
  FieldName ->
  m c a ->
  m c a
setSelectionName fieldname = setScope update
  where
    update ctx = ctx {fieldname}

askSchema ::
  ( MonadContext m c,
    GetWith c (Schema s)
  ) =>
  m c (Schema s)
askSchema = get

askVariables ::
  ( MonadContext m c,
    GetWith c (VariableDefinitions VALID)
  ) =>
  m c (VariableDefinitions VALID)
askVariables = get

askFragments ::
  ( MonadContext m c,
    GetWith c Fragments
  ) =>
  m c Fragments
askFragments = get

runValidator :: Validator ctx a -> Scope -> ctx -> Eventless a
runValidator (Validator x) scope unValidatorContext =
  runReaderT
    x
    ValidatorContext {scope, unValidatorContext}

withContext ::
  (c' -> c) ->
  Validator c a ->
  Validator c' a
withContext f = Validator . withReaderT (fmap f) . _runValidator

withDirective ::
  ( MonadContext m c
  ) =>
  Directive s ->
  m c a ->
  m c a
withDirective
  Directive
    { directiveName,
      directivePosition
    } = setSelectionName directiveName . setScope update
    where
      update Scope {..} =
        Scope
          { position = Just directivePosition,
            kind = DIRECTIVE,
            ..
          }

withScope ::
  ( MonadContext m c
  ) =>
  TypeDefinition cat s ->
  Ref ->
  m c a ->
  m c a
withScope t@TypeDefinition {typeName} (Ref selName pos) =
  setSelectionName selName . setScope update
  where
    update Scope {..} =
      Scope
        { currentTypeName = typeName,
          currentTypeKind = kindOf t,
          position = Just pos,
          ..
        }

withScopeType ::
  ( MonadContext m c
  ) =>
  TypeDefinition cat s ->
  m c a ->
  m c a
withScopeType t@TypeDefinition {typeName} = setScope update
  where
    update Scope {..} =
      Scope
        { currentTypeName = typeName,
          currentTypeKind = kindOf t,
          ..
        }

withPosition ::
  ( MonadContext m c
  ) =>
  Position ->
  m c a ->
  m c a
withPosition pos = setScope update
  where
    update Scope {..} = Scope {position = Just pos, ..}

inputMessagePrefix :: InputValidator ctx Message
inputMessagePrefix =
  renderInputPrefix
    . unValidatorContext <$> Validator ask

startInput ::
  InputSource ->
  InputValidator ctx a ->
  Validator ctx a
startInput inputSource = withContext update
  where
    update sourceContext =
      InputContext
        { inputSource,
          inputPath = [],
          sourceContext
        }

data ValidatorContext (ctx :: *) = ValidatorContext
  { scope :: Scope,
    unValidatorContext :: ctx
  }
  deriving (Show, Functor)

newtype Validator ctx a = Validator
  { _runValidator ::
      ReaderT
        (ValidatorContext ctx)
        Eventless
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad
    )

instance MonadReader ctx (Validator ctx) where
  ask = unValidatorContext <$> Validator ask
  local = withContext

type BaseValidator = Validator (OperationContext ())

type SelectionValidator = Validator (OperationContext (VariableDefinitions VALID))

type InputValidator ctx = Validator (InputContext ctx)

type DirectiveValidator ctx = Validator ctx

setScope ::
  (MonadContext m c) =>
  (Scope -> Scope) ->
  m c b ->
  m c b
setScope f = setGlobalContext (mapScope f)

mapScope :: (Scope -> Scope) -> ValidatorContext s -> ValidatorContext s
mapScope f ValidatorContext {scope, ..} = ValidatorContext {scope = f scope, ..}

-- Helpers
get :: (MonadContext m ctx, GetWith ctx a) => m ctx a
get = getContext getWith

class
  Monad (m c) =>
  MonadContext m c
  where
  getGlobalContext :: (ValidatorContext c -> a) -> m c a
  setGlobalContext :: (ValidatorContext c -> ValidatorContext c) -> m c b -> m c b
  getContext :: (c -> a) -> m c a
  setContext :: (c -> c) -> m c b -> m c b

instance MonadContext Validator c where
  getGlobalContext f = f <$> Validator ask
  getContext f = f . unValidatorContext <$> Validator ask
  setGlobalContext f = Validator . withReaderT f . _runValidator
  setContext = withContext

class GetWith (c :: *) (v :: *) where
  getWith :: c -> v

instance GetWith (OperationContext c) (Schema VALID) where
  getWith = schema

instance GetWith c (Schema s) => GetWith (InputContext c) (Schema s) where
  getWith = getWith . sourceContext

instance GetWith (OperationContext (VariableDefinitions VALID)) (VariableDefinitions VALID) where
  getWith = variables

instance GetWith (InputContext ctx) InputSource where
  getWith = inputSource

instance GetWith (OperationContext v) Fragments where
  getWith = fragments

-- Setters
class SetWith (c :: *) (v :: *) where
  setWith :: (v -> v) -> c -> c

instance SetWith (OperationContext v) CurrentSelection where
  setWith f OperationContext {selection = selection, ..} =
    OperationContext
      { selection = f selection,
        ..
      }

instance Failure GQLErrors (Validator ctx) where
  failure = Validator . lift . failure

-- can be only used for internal errors
instance
  (MonadContext Validator ctx) =>
  Failure InternalError (Validator ctx)
  where
  failure inputMessage = do
    scope <- asksScope id
    failure
      ( renderErrorMessage
          (position scope)
          $ msg
            inputMessage
            <> renderContext scope
      )

renderContext :: Scope -> Message
renderContext
  Scope
    { currentTypeName,
      currentTypeKind,
      fieldname
    } =
    renderSection "current type" currentTypeName
      <> renderSection "current type kind" currentTypeKind
      <> renderSection "current field name" fieldname

renderSection :: RenderGQL a => Message -> a -> Message
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
