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
    Context (..),
    InputSource (..),
    InputContext (..),
    SelectionContext (..),
    renderInputPrefix,
    Target (..),
    Prop (..),
    Resolution,
    ScopeKind (..),
    inputValueSource,
    Scope (..),
    withDirective,
    startInput,
    CTX (..),
    WithSchema (..),
    WithSelection (..),
    WithVariables (..),
    WithScope (..),
  )
where

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
  ( Semigroup (..),
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

data Context = Context
  { schema :: Schema,
    fragments :: Fragments,
    scope :: Scope,
    operationName :: Maybe FieldName,
    currentSelectionName :: FieldName
  }
  deriving (Show)

data CTX c = CTX
  { global :: Context,
    local :: c
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

newtype SelectionContext = SelectionContext
  { variables :: VariableDefinitions VALID
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
withInputScope prop = inContext update
  where
    update
      CTX
        { local =
            InputContext
              { inputPath = old,
                ..
              },
          ..
        } =
        CTX
          { local =
              InputContext
                { inputPath = old <> [prop],
                  ..
                },
            ..
          }

inputValueSource :: InputValidator InputSource
inputValueSource = inputSource . local <$> Validator ask

askContext :: Validator ctx ctx
askContext = Validator ask

askScopeTypeName :: WithScope m => m TypeName
askScopeTypeName = typename <$> getScope

askPosition :: WithScope m => m Position
askPosition = position <$> getScope

runValidator :: Validator (CTX ctx) a -> Context -> ctx -> Eventless a
runValidator (Validator x) global local = runReaderT x CTX {global, local}

inContext ::
  (c' -> c) ->
  Validator c a ->
  Validator c' a
inContext f = Validator . withReaderT f . _runValidator

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
inputMessagePrefix = renderInputPrefix . local <$> askContext

startInput :: InputSource -> InputValidator a -> Validator (CTX ctx) a
startInput inputSource = inContext update
  where
    update CTX {..} =
      CTX
        { local =
            InputContext
              { inputSource,
                inputPath = []
              },
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
      Monad
    )

type BaseValidator = Validator (CTX ())

type SelectionValidator = Validator (CTX SelectionContext)

type InputValidator = Validator (CTX InputContext)

-- Helpers
class WithSchema (m :: * -> *) where
  askSchema :: m Schema

instance WithSchema (Validator (CTX ctx)) where
  askSchema = schema . global <$> Validator ask

-- Variables
class WithVariables (m :: * -> *) where
  askVariables :: m (VariableDefinitions VALID)

instance WithVariables (Validator (CTX SelectionContext)) where
  askVariables = variables . local <$> Validator ask

-- Selection
class
  Functor m =>
  WithSelection (m :: * -> *)
  where
  getContext :: m Context
  setSelectionName :: FieldName -> m a -> m a

  getSelectionName :: m FieldName
  getSelectionName = currentSelectionName <$> getContext

  askFragments :: m Fragments
  askFragments = fragments <$> getContext

instance WithSelection (Validator (CTX ctx)) where
  getContext = global <$> Validator ask
  setSelectionName name = inContext update
    where
      update CTX {global = Context {..}, ..} =
        CTX
          { global =
              Context
                { currentSelectionName = name,
                  ..
                },
            ..
          }

class
  Functor m =>
  WithScope (m :: * -> *)
  where
  getScope :: m Scope
  setScope :: (Scope -> Scope) -> m a -> m a

instance WithScope (Validator (CTX ctx)) where
  getScope = scope . global <$> Validator ask
  setScope f = inContext update
    where
      update CTX {global = Context {..}, ..} =
        CTX
          { global =
              Context
                { scope = f scope,
                  ..
                },
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
