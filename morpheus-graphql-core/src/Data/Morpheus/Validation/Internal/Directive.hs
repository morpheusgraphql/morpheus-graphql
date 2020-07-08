{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateDirectives,
    ValidateDirective (..),
  )
where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.List (elem)
import Data.Morpheus.Error (errorMessage, globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    selectBy,
    selectOr,
  )
import Data.Morpheus.Schema.Directives (defaultDirectives)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    CONST,
    Directive (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    Directives,
    FieldName,
    RAW,
    ScalarValue (..),
    Stage,
    VALID,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
    Scope,
    SetWith,
    Validator,
    selectKnown,
    withDirective,
  )
import Data.Morpheus.Validation.Query.Arguments
  ( ArgCTX,
    Validate,
    validateDirectiveArguments,
  )
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (&&),
    (==),
    Bool (..),
    otherwise,
  )

validateDirectives ::
  ValidateDirective s ctx =>
  DirectiveLocation ->
  Directives s ->
  Validator ctx (Directives VALID)
validateDirectives location = traverse (validateDirective location)

class ValidateDirective (s :: Stage) ctx where
  validateDirective :: DirectiveLocation -> Directive s -> Validator ctx (Directive VALID)

instance
  ( SetWith ctx Scope,
    Validate (ArgCTX ctx VALID) RAW ctx
  ) =>
  ValidateDirective RAW ctx
  where
  validateDirective location directive@Directive {directiveArgs, ..} =
    withDirective directive $ do
      directiveDef <- selectKnown directive defaultDirectives
      args <- validateDirectiveArguments directiveDef directiveArgs
      validateDirectiveLocation location directive directiveDef
      pure Directive {directiveArgs = args, ..}

instance
  ( SetWith ctx Scope,
    Validate (ArgCTX ctx VALID) CONST ctx
  ) =>
  ValidateDirective CONST ctx
  where
  validateDirective location directive@Directive {directiveArgs = args, ..} =
    withDirective directive $ do
      directiveDef <- selectKnown directive defaultDirectives
      directiveArgs <- validateDirectiveArguments directiveDef args
      validateDirectiveLocation location directive directiveDef
      pure Directive {..}

validateDirectiveLocation ::
  DirectiveLocation ->
  Directive s ->
  DirectiveDefinition s' ->
  Validator ctx ()
validateDirectiveLocation
  loc
  Directive {directiveName, directivePosition}
  DirectiveDefinition {directiveDefinitionLocations}
    | loc `elem` directiveDefinitionLocations = pure ()
    | otherwise =
      failure $
        errorMessage
          directivePosition
          ("Directive " <> msg directiveName <> " may not to be used on " <> msg loc)

directiveFulfilled ::
  GetWith ctx Scope =>
  Bool ->
  FieldName ->
  Directives s ->
  Validator ctx Bool
directiveFulfilled target = selectOr (pure True) (argumentIf target)

shouldIncludeSelection ::
  GetWith ctx Scope =>
  Directives VALID ->
  Validator ctx Bool
shouldIncludeSelection directives = do
  dontSkip <- directiveFulfilled False "skip" directives
  include <- directiveFulfilled True "include" directives
  pure (dontSkip && include)

argumentIf ::
  GetWith ctx Scope =>
  Bool ->
  Directive s ->
  Validator ctx Bool
argumentIf target Directive {directiveName, directiveArgs} =
  selectBy err "if" directiveArgs
    >>= assertArgument target
  where
    err = globalErrorMessage $ "Directive " <> msg ("@" <> directiveName) <> " argument \"if\" of type \"Boolean!\" is required but not provided."

assertArgument ::
  GetWith ctx Scope =>
  Bool ->
  Argument s ->
  Validator ctx Bool
assertArgument asserted Argument {argumentValue = Scalar (Boolean actual)} = pure (asserted == actual)
assertArgument _ Argument {argumentValue} = failure $ "Expected type Boolean!, found " <> msg argumentValue <> "."
