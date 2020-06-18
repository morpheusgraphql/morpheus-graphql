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

renderInputPrefix :: InputContext -> Message
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

data OperationContext v i = OperationContext
  { schema :: Schema,
    scope :: Scope,
    fragments :: Fragments,
    operationName :: Maybe FieldName,
    currentSelectionName :: FieldName,
    variables :: v,
    input :: i
  }
  deriving (Show)

data Scope = Scope
  { position :: Position,
    typename :: TypeName,
    kind :: ScopeKind
  }
  deriving (Show)

data InputContext = InputContext
  { inputSource :: InputSource,
    inputPath :: [Prop]
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

withInputScope :: Prop -> InputValidator a -> InputValidator a
withInputScope prop = withContext update
  where
    update
      OperationContext
        { input =
            InputContext
              { inputPath = old,
                ..
              },
          ..
        } =
        OperationContext
          { input =
              InputContext
                { inputPath = old <> [prop],
                  ..
                },
            ..
          }

inputValueSource :: InputValidator InputSource
inputValueSource = inputSource . input <$> Validator ask

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

inputMessagePrefix :: InputValidator Message
inputMessagePrefix = renderInputPrefix . input <$> askContext

startInput ::
  InputSource ->
  InputValidator a ->
  Validator (OperationContext v ()) a
startInput inputSource = withContext update
  where
    update OperationContext {..} =
      OperationContext
        { input =
            InputContext
              { inputSource,
                inputPath = []
              },
          variables = (),
          ..
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

type BaseValidator = Validator (OperationContext () ())

type SelectionValidator = Validator (OperationContext (VariableDefinitions VALID) ())

type InputValidator = Validator (OperationContext () InputContext)

-- Helpers
class WithSchema (m :: * -> *) where
  askSchema :: m Schema

instance WithSchema (Validator (OperationContext v i)) where
  askSchema = schema <$> Validator ask

-- Variables
class WithVariables (m :: * -> *) where
  askVariables :: m (VariableDefinitions VALID)

instance WithVariables (Validator (OperationContext (VariableDefinitions VALID) i)) where
  askVariables = variables <$> Validator ask

-- Selection
class
  Functor m =>
  WithSelection (m :: * -> *)
  where
  setSelectionName :: FieldName -> m a -> m a
  getSelectionName :: m FieldName

  askFragments :: m Fragments

instance WithSelection (Validator (OperationContext v i)) where
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

instance WithScope (Validator (OperationContext v i)) where
  getScope = scope <$> Validator ask
  setScope f = withContext update
    where
      update OperationContext {..} =
        OperationContext
          { scope = f scope,
            ..
          }

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

instance Failure GQLErrors (Validator ctx) where
  failure = Validator . lift . failure
