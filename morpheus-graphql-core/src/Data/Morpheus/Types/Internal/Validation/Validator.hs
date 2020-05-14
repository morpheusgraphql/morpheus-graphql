{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Validation.Validator
  ( Validator (..),
    SelectionValidator,
    InputValidator,
    BaseValidator,
    runValidator,
    askSchema,
    askContext,
    askFragments,
    Constraint (..),
    withScope,
    withScopeType,
    withScopePosition,
    askScopeTypeName,
    askScopePosition,
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
  )
where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    ask,
    withReaderT,
  )
-- MORPHEUS

import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    FieldName,
    FieldsDefinition (..),
    Fragments,
    GQLError (..),
    GQLErrors,
    IN,
    Message,
    Position,
    RAW,
    RESOLVED,
    Schema,
    TypeDefinition (..),
    TypeName,
    VALID,
    Variable (..),
    VariableDefinitions,
    intercalateName,
    msg,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup
  ( (<>),
    Semigroup (..),
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
renderSource (SourceVariable Variable {variableName}) =
  "Variable " <> msg ("$" <> variableName) <> " got invalid value. "

data Context = Context
  { schema :: Schema,
    fragments :: Fragments,
    scopePosition :: Position,
    scopeTypeName :: TypeName,
    operationName :: Maybe FieldName,
    scopeSelectionName :: FieldName
  }
  deriving (Show)

data InputContext = InputContext
  { inputSource :: InputSource,
    inputPath :: [Prop]
  }
  deriving (Show)

data InputSource
  = SourceArgument (Argument RESOLVED)
  | SourceVariable (Variable RAW)
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

type instance Resolution 'TARGET_OBJECT = (TypeName, FieldsDefinition)

type instance Resolution 'TARGET_INPUT = TypeDefinition IN

--type instance Resolution 'TARGET_UNION = DataUnion

withInputScope :: Prop -> InputValidator a -> InputValidator a
withInputScope prop = setContext update
  where
    update ctx@InputContext {inputPath = old} =
      ctx {inputPath = old <> [prop]}

askContext :: Validator ctx ctx
askContext = snd <$> Validator ask

askSchema :: Validator ctx Schema
askSchema = schema . fst <$> Validator ask

askFragments :: Validator ctx Fragments
askFragments = fragments . fst <$> Validator ask

askScopeTypeName :: Validator ctx TypeName
askScopeTypeName = scopeTypeName . fst <$> Validator ask

askScopePosition :: Validator ctx Position
askScopePosition = scopePosition . fst <$> Validator ask

setContext ::
  (c' -> c) ->
  Validator c a ->
  Validator c' a
setContext f = Validator . withReaderT (\(x, y) -> (x, f y)) . _runValidator

setGlobalContext ::
  (Context -> Context) ->
  Validator c a ->
  Validator c a
setGlobalContext f = Validator . withReaderT (\(x, y) -> (f x, y)) . _runValidator

withScope :: TypeName -> Position -> Validator ctx a -> Validator ctx a
withScope scopeTypeName scopePosition = setGlobalContext update
  where
    update ctx = ctx {scopeTypeName, scopePosition}

withScopePosition :: Position -> Validator ctx a -> Validator ctx a
withScopePosition scopePosition = setGlobalContext update
  where
    update ctx = ctx {scopePosition}

withScopeType :: TypeName -> Validator ctx a -> Validator ctx a
withScopeType scopeTypeName = setGlobalContext update
  where
    update ctx = ctx {scopeTypeName}

inputMessagePrefix :: InputValidator Message
inputMessagePrefix = renderInputPrefix <$> askContext

runValidator :: Validator ctx a -> Context -> ctx -> Eventless a
runValidator (Validator x) globalCTX ctx = runReaderT x (globalCTX, ctx)

newtype Validator ctx a = Validator
  { _runValidator ::
      ReaderT
        (Context, ctx)
        Eventless
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad
    )

type BaseValidator = Validator ()

type SelectionValidator = Validator SelectionContext

type InputValidator = Validator InputContext

-- can be only used for internal errors
instance Failure Message (Validator ctx) where
  failure inputMessage = do
    position <- askScopePosition
    failure
      [ GQLError
          { message = "INTERNAL: " <> inputMessage,
            locations = [position]
          }
      ]

instance Failure GQLErrors (Validator ctx) where
  failure = Validator . lift . failure
