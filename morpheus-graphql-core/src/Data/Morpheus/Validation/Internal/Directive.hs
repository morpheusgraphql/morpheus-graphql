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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateQueryDirectives,
    validateTypeSystemDirectives,
  )
where

import Control.Applicative ((*>), pure)
import Control.Monad ((>>=))
import Data.Functor ((<$>))
import Data.List (elem)
import Data.Morpheus.Error (errorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    CONST,
    Directive (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    Directives,
    FieldName,
    ScalarValue (..),
    Schema,
    Schema (..),
    VALID,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
    Validator,
    askSchema,
    selectKnown,
    withDirective,
  )
import Data.Morpheus.Validation.Internal.Arguments
  ( ArgumentsConstraints,
    Resolve,
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

type DirectiveConstraint ctx schemaS s =
  ( Resolve Argument s ctx,
    GetWith ctx (Schema schemaS)
  )

validateQueryDirectives ::
  DirectiveConstraint ctx VALID s =>
  DirectiveLocation ->
  Directives s ->
  Validator VALID ctx (Directives VALID)
validateQueryDirectives location = traverse (validate location)

validateTypeSystemDirectives ::
  DirectiveConstraint ctx CONST s =>
  DirectiveLocation ->
  Directives s ->
  Validator CONST ctx (Directives VALID)
validateTypeSystemDirectives location = traverse (validate location)

validate ::
  forall s c schemaS.
  ArgumentsConstraints c schemaS s =>
  DirectiveLocation ->
  Directive s ->
  Validator schemaS c (Directive VALID)
validate location directive@Directive {..} =
  withDirective directive $ do
    (Schema {directiveDefinitions} :: Schema schemaS) <- askSchema
    (directiveDef :: DirectiveDefinition schemaS) <- selectKnown directive directiveDefinitions
    Directive directiveName directivePosition
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
      failure $
        errorMessage
          directivePosition
          ("Directive " <> msg directiveName <> " may not to be used on " <> msg loc)

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
  dontSkip <- directiveFulfilled False "skip" directives
  include <- directiveFulfilled True "include" directives
  pure (dontSkip && include)

argumentIf ::
  Bool ->
  Directive s ->
  Validator schemaS ctx Bool
argumentIf target Directive {directiveName, directiveArgs, directivePosition} =
  selectBy err "if" directiveArgs
    >>= assertArgument target
  where
    err =
      errorMessage directivePosition $
        "Directive "
          <> msg ("@" <> directiveName)
          <> " argument \"if\" of type \"Boolean!\" is required but not provided."

assertArgument ::
  Bool ->
  Argument s ->
  Validator schemaS ctx Bool
assertArgument asserted Argument {argumentValue = Scalar (Boolean actual)} = pure (asserted == actual)
assertArgument _ Argument {argumentValue, argumentPosition} =
  failure
    $ errorMessage argumentPosition
    $ "Expected type Boolean!, found " <> msg argumentValue <> "."
