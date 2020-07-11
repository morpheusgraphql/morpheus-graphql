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
{-# LANGUAGE TypeApplications #-}
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
import Data.Proxy (Proxy (..))
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
  Validator ctx (Directives VALID)
validateQueryDirectives location = traverse (validate (Proxy @VALID) location)

validateTypeSystemDirectives ::
  DirectiveConstraint ctx CONST s =>
  DirectiveLocation ->
  Directives s ->
  Validator ctx (Directives VALID)
validateTypeSystemDirectives location = traverse (validate (Proxy @CONST) location)

validate ::
  forall s c schemaS.
  ArgumentsConstraints c schemaS s =>
  Proxy schemaS ->
  DirectiveLocation ->
  Directive s ->
  Validator c (Directive VALID)
validate _ location directive@Directive {..} =
  withDirective directive $ do
    (Schema {directiveDefinitions} :: Schema schemaS) <- askSchema
    (directiveDef :: DirectiveDefinition schemaS) <-
      selectKnown
        directive
        (directiveDefinitions <> defaultDirectives)
    Directive directiveName directivePosition
      <$> ( validateDirectiveLocation location directive directiveDef
              *> validateDirectiveArguments directiveDef directiveArgs
          )

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
  Bool ->
  FieldName ->
  Directives s ->
  Validator ctx Bool
directiveFulfilled target = selectOr (pure True) (argumentIf target)

shouldIncludeSelection ::
  Directives VALID ->
  Validator ctx Bool
shouldIncludeSelection directives = do
  dontSkip <- directiveFulfilled False "skip" directives
  include <- directiveFulfilled True "include" directives
  pure (dontSkip && include)

argumentIf ::
  Bool ->
  Directive s ->
  Validator ctx Bool
argumentIf target Directive {directiveName, directiveArgs} =
  selectBy err "if" directiveArgs
    >>= assertArgument target
  where
    err = globalErrorMessage $ "Directive " <> msg ("@" <> directiveName) <> " argument \"if\" of type \"Boolean!\" is required but not provided."

assertArgument ::
  Bool ->
  Argument s ->
  Validator ctx Bool
assertArgument asserted Argument {argumentValue = Scalar (Boolean actual)} = pure (asserted == actual)
assertArgument _ Argument {argumentValue} = failure $ "Expected type Boolean!, found " <> msg argumentValue <> "."
