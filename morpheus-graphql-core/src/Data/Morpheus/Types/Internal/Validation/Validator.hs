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
    askContext,
    Constraint (..),
    withScope,
    withScopeType,
    withPosition,
    askScopeTypeName,
    askPosition,
    withInputScope,
    inputMessagePrefix,
    InputSource (..),
    InputContext (..),
    OperationContext (..),
    renderInputPrefix,
    Target (..),
    Prop (..),
    Resolution,
    ScopeKind (..),
    inputValueSource,
    Scope (..),
    withDirective,
    startInput,
    WithSchema (..),
    WithSelection (..),
    WithVariables (..),
    WithScope (..),
    WithInput,
    withContext,
  )
where

import Control.Monad.Reader (MonadReader, asks)
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
    FieldName,
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
    TypeName,
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

data ScopeKind
  = DIRECTIVE
  | SELECTION
  deriving (Show)

data OperationContext vars = OperationContext
  { schema :: Schema,
    scope :: Scope,
    fragments :: Fragments,
    operationName :: Maybe FieldName,
    currentSelectionName :: FieldName,
    variables :: vars
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

inputValueSource :: (WithInput m, Functor m) => m InputSource
inputValueSource = inputSource <$> askInput

askContext :: Validator ctx ctx
askContext = Validator ask

askScopeTypeName :: WithScope m => m TypeName
askScopeTypeName = typename <$> getScope

askPosition :: WithScope m => m Position
askPosition = position <$> getScope

runValidator :: Validator ctx a -> ctx -> Eventless a
runValidator (Validator x) = runReaderT x

withContext ::
  (c' -> c) ->
  Validator c a ->
  Validator c' a
withContext f = Validator . withReaderT f . _runValidator

withDirective ::
  ( WithSelection m,
    WithScope m
  ) =>
  Directive s ->
  m a ->
  m a
withDirective
  Directive
    { directiveName,
      directivePosition
    } = setSelectionName directiveName . setScope update
    where
      update Scope {..} =
        Scope
          { position = directivePosition,
            kind = DIRECTIVE,
            ..
          }

withScope ::
  ( WithSelection m,
    WithScope m
  ) =>
  TypeName ->
  Ref ->
  m a ->
  m a
withScope typeName (Ref selName position) = setSelectionName selName . withScope' typeName position

withScope' :: WithScope m => TypeName -> Position -> m a -> m a
withScope' name pos = setScope update
  where
    update Scope {..} = Scope {typename = name, position = pos, ..}

withPosition :: WithScope m => Position -> m a -> m a
withPosition pos = setScope update
  where
    update Scope {..} = Scope {position = pos, ..}

withScopeType :: WithScope m => TypeName -> m a -> m a
withScopeType name = setScope update
  where
    update Scope {..} = Scope {typename = name, ..}

inputMessagePrefix :: InputValidator ctx Message
inputMessagePrefix = renderInputPrefix <$> askContext

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

toInput ::
  Validator ctx a ->
  InputValidator ctx a
toInput = withContext sourceContext

-- Helpers
class WithSchema (m :: * -> *) where
  askSchema :: m Schema

instance WithSchema (Validator (OperationContext v)) where
  askSchema = schema <$> Validator ask

instance
  WithSchema (Validator ctx) =>
  WithSchema (Validator (InputContext ctx))
  where
  askSchema = toInput askSchema

-- Variables
class WithVariables (m :: * -> *) where
  askVariables :: m (VariableDefinitions VALID)

instance WithVariables (Validator (OperationContext (VariableDefinitions VALID))) where
  askVariables = variables <$> Validator ask

-- Selection
class
  Functor m =>
  WithSelection (m :: * -> *)
  where
  setSelectionName :: FieldName -> m a -> m a
  getSelectionName :: m FieldName

  askFragments :: m Fragments

instance WithSelection (Validator (OperationContext v)) where
  askFragments = fragments <$> Validator ask
  getSelectionName = currentSelectionName <$> Validator ask
  setSelectionName name = withContext update
    where
      update OperationContext {..} =
        OperationContext
          { currentSelectionName = name,
            ..
          }

class
  Functor m =>
  WithScope (m :: * -> *)
  where
  getScope :: m Scope
  setScope :: (Scope -> Scope) -> m a -> m a

instance WithScope (Validator (OperationContext v)) where
  getScope = scope <$> Validator ask
  setScope f = withContext update
    where
      update OperationContext {..} =
        OperationContext
          { scope = f scope,
            ..
          }

instance WithScope (Validator c) => WithScope (Validator (InputContext c)) where
  getScope = toInput getScope

-- can be only used for internal errors
instance
  WithScope (Validator ctx) =>
  Failure Message (Validator ctx)
  where
  failure inputMessage = do
    Scope {position} <- getScope
    failure
      [ GQLError
          { message = "INTERNAL: " <> inputMessage,
            locations = [position]
          }
      ]

class
  Applicative m =>
  WithInput (m :: * -> *)
  where
  askInput :: m (InputContext ())

instance WithInput (Validator (InputContext ctx)) where
  askInput = (\x -> x {sourceContext = ()}) <$> Validator ask

instance Failure GQLErrors (Validator ctx) where
  failure = Validator . lift . failure
