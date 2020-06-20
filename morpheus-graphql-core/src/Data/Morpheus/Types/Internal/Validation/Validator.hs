{-# LANGUAGE DataKinds #-}
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
    askSchema,
    askVariables,
    askFragments,
  )
where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    ask,
    withReaderT,
  )
-- MORPHEUS
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Directive (..),
    FieldName (..),
    FieldsDefinition,
    Fragments,
    GQLError (..),
    GQLErrors,
    IN,
    Message,
    OUT,
    Position,
    RAW,
    RESOLVED,
    Ref (..),
    Schema,
    TypeDefinition,
    TypeName (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    intercalateName,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup
  ( (<>),
  )

data Prop = Prop
  { propName :: FieldName,
    propTypeName :: TypeName
  }
  deriving (Show)

type Path = [Prop]

renderPath :: Path -> Message
renderPath [] = ""
renderPath path = "in field " <> msg (intercalateName "." $ map propName path) <> ": "

renderInputPrefix :: InputContext c -> Message
renderInputPrefix InputContext {inputPath, inputSource} =
  renderSource inputSource <> renderPath inputPath

renderSource :: InputSource -> Message
renderSource (SourceArgument Argument {argumentName}) =
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
  { schema :: Schema,
    scope :: Scope,
    fragments :: Fragments,
    selection :: CurrentSelection,
    variables :: vars
  }
  deriving (Show)

data CurrentSelection = CurrentSelection
  { operationName :: Maybe FieldName,
    selectionName :: FieldName
  }
  deriving (Show)

data Scope = Scope
  { position :: Position,
    typename :: TypeName,
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
  = SourceArgument (Argument RESOLVED)
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

type family Resolution (a :: Target)

type instance Resolution 'TARGET_OBJECT = (TypeName, FieldsDefinition OUT)

type instance Resolution 'TARGET_INPUT = TypeDefinition IN

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

setSelectionName ::
  ( MonadContext m c,
    SetWith c CurrentSelection
  ) =>
  FieldName ->
  m c a ->
  m c a
setSelectionName selectionName = set update
  where
    update ctx = ctx {selectionName}

askSchema ::
  ( MonadContext m c,
    GetWith c Schema
  ) =>
  m c Schema
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

runValidator :: Validator ctx a -> ctx -> Eventless a
runValidator (Validator x) = runReaderT x

withContext ::
  (c' -> c) ->
  Validator c a ->
  Validator c' a
withContext f = Validator . withReaderT f . _runValidator

withDirective ::
  ( SetWith c CurrentSelection,
    SetWith c Scope,
    MonadContext m c
  ) =>
  Directive s ->
  m c a ->
  m c a
withDirective
  Directive
    { directiveName,
      directivePosition
    } = setSelectionName directiveName . set update
    where
      update Scope {..} =
        Scope
          { position = directivePosition,
            kind = DIRECTIVE,
            ..
          }

withScope ::
  ( SetWith c CurrentSelection,
    MonadContext m c,
    SetWith c Scope
  ) =>
  TypeName ->
  Ref ->
  m c a ->
  m c a
withScope typeName (Ref selName pos) =
  setSelectionName selName . set update
  where
    update Scope {..} = Scope {typename = typeName, position = pos, ..}

withPosition ::
  ( MonadContext m c,
    SetWith c Scope
  ) =>
  Position ->
  m c a ->
  m c a
withPosition pos = set update
  where
    update Scope {..} = Scope {position = pos, ..}

withScopeType ::
  ( MonadContext m c,
    SetWith c Scope
  ) =>
  TypeName ->
  m c a ->
  m c a
withScopeType name = set update
  where
    update Scope {..} = Scope {typename = name, ..}

inputMessagePrefix :: InputValidator ctx Message
inputMessagePrefix = renderInputPrefix <$> Validator ask

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

newtype Validator ctx a = Validator
  { _runValidator ::
      ReaderT
        ctx
        Eventless
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader ctx
    )

type BaseValidator = Validator (OperationContext ())

type SelectionValidator = Validator (OperationContext (VariableDefinitions VALID))

type InputValidator ctx = Validator (InputContext ctx)

-- Helpers
get :: (MonadContext m ctx, GetWith ctx a) => m ctx a
get = getContext getWith

set ::
  ( MonadContext m c,
    SetWith c a
  ) =>
  (a -> a) ->
  m c b ->
  m c b
set f = setContext (setWith f)

class
  Monad (m c) =>
  MonadContext m c
  where
  getContext :: (c -> a) -> m c a
  setContext :: (c -> c) -> m c b -> m c b

instance MonadContext Validator c where
  getContext f = f <$> Validator ask
  setContext = withContext

class GetWith (c :: *) (v :: *) where
  getWith :: c -> v

instance GetWith (OperationContext v) Scope where
  getWith = scope

instance GetWith c Scope => GetWith (InputContext c) Scope where
  getWith = getWith . sourceContext

instance GetWith (OperationContext c) Schema where
  getWith = schema

instance GetWith c Schema => GetWith (InputContext c) Schema where
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

instance SetWith (OperationContext v) Scope where
  setWith f OperationContext {..} =
    OperationContext
      { scope = f scope,
        ..
      }

instance SetWith c Scope => SetWith (InputContext c) Scope where
  setWith f InputContext {..} =
    InputContext
      { sourceContext = setWith f sourceContext,
        ..
      }

-- can be only used for internal errors
instance
  ( MonadContext Validator ctx,
    GetWith ctx Scope
  ) =>
  Failure Message (Validator ctx)
  where
  failure inputMessage = do
    position <- asks position
    failure
      [ GQLError
          { message = "INTERNAL: " <> inputMessage,
            locations = [position]
          }
      ]

instance Failure GQLErrors (Validator ctx) where
  failure = Validator . lift . failure
