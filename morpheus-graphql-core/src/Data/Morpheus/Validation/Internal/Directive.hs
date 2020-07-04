{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    Directive (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    Directives,
    FieldName,
    RAW,
    ScalarValue (..),
    VALID,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Validation
  ( DirectiveValidator,
    selectKnown,
    withDirective,
  )
import Data.Morpheus.Validation.Query.Arguments
  ( validateDirectiveArguments,
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

validateDirective :: DirectiveLocation -> [DirectiveDefinition] -> Directive RAW -> DirectiveValidator ctx (Directive VALID)
validateDirective location directiveDefs directive@Directive {directiveArgs, ..} =
  withDirective directive $ do
    directiveDef <- selectKnown directive directiveDefs
    args <- validateDirectiveArguments directiveDef directiveArgs
    validateDirectiveLocation location directive directiveDef
    pure Directive {directiveArgs = args, ..}

validateDirectiveLocation ::
  DirectiveLocation ->
  Directive s ->
  DirectiveDefinition ->
  DirectiveValidator ctx ()
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

validateDirectives :: DirectiveLocation -> Directives RAW -> DirectiveValidator ctx (Directives VALID)
validateDirectives location = traverse (validateDirective location defaultDirectives)

directiveFulfilled :: Bool -> FieldName -> Directives s -> DirectiveValidator ctx Bool
directiveFulfilled target = selectOr (pure True) (argumentIf target)

shouldIncludeSelection :: Directives VALID -> DirectiveValidator ctx Bool
shouldIncludeSelection directives = do
  dontSkip <- directiveFulfilled False "skip" directives
  include <- directiveFulfilled True "include" directives
  pure (dontSkip && include)

argumentIf :: Bool -> Directive s -> DirectiveValidator ctx Bool
argumentIf target Directive {directiveName, directiveArgs} =
  selectBy err "if" directiveArgs
    >>= assertArgument target
  where
    err = globalErrorMessage $ "Directive " <> msg ("@" <> directiveName) <> " argument \"if\" of type \"Boolean!\" is required but not provided."

assertArgument :: Bool -> Argument s -> DirectiveValidator ctx Bool
assertArgument asserted Argument {argumentValue = Scalar (Boolean actual)} = pure (asserted == actual)
assertArgument _ Argument {argumentValue} = failure $ "Expected type Boolean!, found " <> msg argumentValue <> "."
