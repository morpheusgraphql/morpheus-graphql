{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Arguments
  ( validateDirectiveArguments,
    validateFieldArguments,
    ArgumentsConstraints,
    Resolve,
  )
where

import Control.Applicative ((*>), pure)
import Control.Monad ((>=>), (>>=))
import Data.Functor ((<$>), fmap)
import Data.Maybe (fromMaybe, maybe)
import Data.Morpheus.Internal.Utils
  ( empty,
    ordTraverse,
    ordTraverse_,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    ArgumentDefinition,
    Arguments,
    ArgumentsDefinition (..),
    CONST,
    DirectiveDefinition,
    DirectiveDefinition (..),
    FieldDefinition (..),
    IN,
    OUT,
    ObjectEntry (..),
    Position (..),
    RAW,
    Schema,
    TypedRef (..),
    VALID,
    Value (..),
    VariableDefinitions,
    fieldContentArgs,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
    InputSource (..),
    MissingRequired,
    Scope (..),
    Validator,
    askVariables,
    asksScope,
    selectKnown,
    selectRequired,
    selectWithDefaultValue,
    startInput,
    withPosition,
  )
import Data.Morpheus.Validation.Internal.Value
  ( ValidateWithDefault,
    ValueConstraints,
    validateInputByTypeRef,
  )
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (.),
    flip,
  )

type VariableConstraints ctx =
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx
  )

type ArgumentsConstraints c schemaS valueS =
  ( Resolve Argument valueS c,
    GetWith c (Schema schemaS),
    ValidateWithDefault c schemaS schemaS,
    ValidateWithDefault c schemaS CONST
  )

validateArgument ::
  forall ctx schemaS valueS.
  ( ValueConstraints ctx schemaS valueS,
    ValidateWithDefault ctx schemaS schemaS
  ) =>
  Arguments valueS ->
  FieldDefinition IN schemaS ->
  Validator ctx (Argument VALID)
validateArgument
  requestArgs
  argumentDef =
    selectWithDefaultValue
      (toArgument argumentDef >=> validateArgumentValue argumentDef)
      (validateArgumentValue argumentDef)
      argumentDef
      requestArgs

toArgument :: FieldDefinition IN s -> Value schemaS -> Validator ctx (Argument schemaS)
toArgument
  FieldDefinition {fieldName}
  value = flip (Argument fieldName) value . fromMaybe (Position 0 0) <$> asksScope position

validateArgumentValue ::
  forall ctx schemaS valueS.
  (ValueConstraints ctx schemaS valueS) =>
  FieldDefinition IN schemaS ->
  Argument valueS ->
  Validator ctx (Argument VALID)
validateArgumentValue
  FieldDefinition {fieldType}
  Argument {argumentValue = value, ..} =
    withPosition argumentPosition
      $ startInput (SourceArgument argumentName)
      $ do
        argumentValue <- validateInputByTypeRef (TypedRef fieldType :: TypedRef IN schemaS) value
        pure Argument {argumentValue, ..}

validateFieldArguments ::
  forall ctx.
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx,
    GetWith ctx (Schema VALID)
  ) =>
  FieldDefinition OUT VALID ->
  Arguments RAW ->
  Validator ctx (Arguments VALID)
validateFieldArguments fieldDef@FieldDefinition {fieldContent} =
  validateArguments
    (`selectKnown` fieldDef)
    argsDef
  where
    argsDef = maybe empty fieldContentArgs fieldContent

validateDirectiveArguments ::
  forall ctx schemaStage valueStage.
  ArgumentsConstraints ctx schemaStage valueStage =>
  DirectiveDefinition schemaStage ->
  Arguments valueStage ->
  Validator ctx (Arguments VALID)
validateDirectiveArguments
  directiveDef@DirectiveDefinition
    { directiveDefinitionArgs
    } =
    validateArguments
      (`selectKnown` directiveDef)
      directiveDefinitionArgs

validateArguments ::
  ArgumentsConstraints ctx schemaStage s =>
  (Argument CONST -> Validator ctx (ArgumentDefinition schemaStage)) ->
  ArgumentsDefinition schemaStage ->
  Arguments s ->
  Validator ctx (Arguments VALID)
validateArguments checkUnknown argsDef rawArgs = do
  args <- ordTraverse resolve rawArgs
  ordTraverse_ checkUnknown args
    *> ordTraverse (validateArgument args) (arguments argsDef)

class Resolve f s ctx where
  resolve :: f s -> Validator ctx (f CONST)

instance VariableConstraints ctx => Resolve Argument RAW ctx where
  resolve (Argument key position val) = Argument key position <$> resolve val

instance Resolve f CONST ctx where
  resolve = pure

instance VariableConstraints ctx => Resolve Value RAW ctx where
  resolve Null = pure Null
  resolve (Scalar x) = pure $ Scalar x
  resolve (Enum x) = pure $ Enum x
  resolve (List elems) = List <$> traverse resolve elems
  resolve (Object fields) = Object <$> traverse resolve fields
  resolve (VariableValue ref) =
    askVariables
      >>= fmap (ResolvedVariable ref)
        . selectRequired ref

instance VariableConstraints ctx => Resolve ObjectEntry RAW ctx where
  resolve (ObjectEntry name value) = ObjectEntry name <$> resolve value
