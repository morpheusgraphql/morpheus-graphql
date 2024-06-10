{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateDirectives,
  )
where

import Control.Monad.Except (throwError)
import Data.Morpheus.Internal.Utils
  ( selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Directive (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    Directives,
    FieldName,
    Ref (..),
    ScalarValue (..),
    Schema (..),
    VALID,
    Value (..),
    at,
    msg,
  )
import Data.Morpheus.Types.Internal.Validation
  ( Validator,
    ValidatorContext (schema),
    selectKnown,
    selectRequired,
    setDirective,
    withScope,
  )
import Data.Morpheus.Validation.Internal.Arguments
  ( ArgumentsConstraints,
    validateDirectiveArguments,
  )
import Relude

validateDirectives ::
  (ArgumentsConstraints ctx schemaS s) =>
  DirectiveLocation ->
  Directives s ->
  Validator schemaS ctx (Directives VALID)
validateDirectives location = traverse (validate location)

validate ::
  (ArgumentsConstraints c schemaS s) =>
  DirectiveLocation ->
  Directive s ->
  Validator schemaS c (Directive VALID)
validate location directive@Directive {..} =
  withScope (setDirective directive) $ do
    directiveDefinitions <- asks (directiveDefinitions . schema)
    directiveDef <- selectKnown directive directiveDefinitions
    Directive directivePosition directiveName
      <$> ( validateDirectiveLocation location directive directiveDef
              *> validateDirectiveArguments directiveDef directiveArgs
          )

validateDirectiveLocation ::
  DirectiveLocation ->
  Directive s ->
  DirectiveDefinition s' ->
  Validator schemaS ctx ()
validateDirectiveLocation
  loc
  Directive {directiveName, directivePosition}
  DirectiveDefinition {directiveDefinitionLocations}
    | loc `elem` directiveDefinitionLocations = pure ()
    | otherwise =
        throwError
          $ ("Directive " <> msg directiveName <> " may not to be used on " <> msg loc)
          `at` directivePosition

directiveFulfilled ::
  Bool ->
  FieldName ->
  Directives s ->
  Validator schemaS ctx Bool
directiveFulfilled target = selectOr (pure True) (argumentIf target)

shouldIncludeSelection ::
  Directives VALID ->
  Validator schemaS ctx Bool
shouldIncludeSelection directives = do
  doNotSkip <- directiveFulfilled False "skip" directives
  include <- directiveFulfilled True "include" directives
  pure (doNotSkip && include)

argumentIf ::
  Bool ->
  Directive s ->
  Validator schemaS ctx Bool
argumentIf target Directive {directiveArgs, directivePosition} =
  selectRequired (Ref "if" directivePosition) directiveArgs
    >>= assertArgument target

assertArgument ::
  Bool ->
  Argument s ->
  Validator schemaS ctx Bool
assertArgument asserted Argument {argumentValue = Scalar (Boolean actual)} = pure (asserted == actual)
assertArgument _ Argument {argumentValue, argumentPosition} =
  throwError
    $ ( "Expected type Boolean!, found "
          <> msg argumentValue
          <> "."
      )
    `at` argumentPosition
