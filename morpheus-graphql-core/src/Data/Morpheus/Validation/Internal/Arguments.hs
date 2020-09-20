{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
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
    VALID,
    Value (..),
    VariableDefinitions,
    fieldContentArgs,
    typed,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    GetWith,
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
    validateInputByTypeRef,
  )
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (.),
  )

type VariableConstraints ctx =
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx
  )

type ArgumentsConstraints c schemaS valueS =
  ( Resolve Argument valueS c,
    ValidateWithDefault c schemaS schemaS,
    ValidateWithDefault c schemaS CONST
  )

validateArgument ::
  ( ValidateWithDefault ctx schemaS valueS,
    ValidateWithDefault ctx schemaS schemaS
  ) =>
  Arguments valueS ->
  FieldDefinition IN schemaS ->
  Validator schemaS ctx (Argument VALID)
validateArgument
  requestArgs
  argumentDef =
    selectWithDefaultValue
      (toArgument argumentDef >=> validateArgumentValue argumentDef)
      (validateArgumentValue argumentDef)
      argumentDef
      requestArgs

toArgument :: FieldDefinition IN s -> Value schemaS -> Validator schemaStage ctx (Argument schemaS)
toArgument
  FieldDefinition {fieldName}
  value = mkArg . fromMaybe (Position 0 0) <$> asksScope position
    where
      mkArg pos = Argument pos fieldName value

validateArgumentValue ::
  (ValidateWithDefault ctx schemaS valueS) =>
  FieldDefinition IN schemaS ->
  Argument valueS ->
  Validator schemaS ctx (Argument VALID)
validateArgumentValue
  field
  Argument {argumentValue, ..} =
    withPosition argumentPosition
      $ startInput (SourceArgument argumentName)
      $ Argument
        argumentPosition
        argumentName
        <$> validateInputByTypeRef (typed fieldType field) argumentValue

validateFieldArguments ::
  FieldDefinition OUT VALID ->
  Arguments RAW ->
  FragmentValidator s (Arguments VALID)
validateFieldArguments fieldDef@FieldDefinition {fieldContent} =
  validateArguments
    (`selectKnown` fieldDef)
    argsDef
  where
    argsDef = maybe empty fieldContentArgs fieldContent

validateDirectiveArguments ::
  ArgumentsConstraints ctx schemaStage valueStage =>
  DirectiveDefinition schemaStage ->
  Arguments valueStage ->
  Validator schemaStage ctx (Arguments VALID)
validateDirectiveArguments
  directiveDef@DirectiveDefinition
    { directiveDefinitionArgs
    } =
    validateArguments
      (`selectKnown` directiveDef)
      directiveDefinitionArgs

validateArguments ::
  ArgumentsConstraints ctx schemaStage s =>
  (Argument CONST -> Validator schemaStage ctx (ArgumentDefinition schemaStage)) ->
  ArgumentsDefinition schemaStage ->
  Arguments s ->
  Validator schemaStage ctx (Arguments VALID)
validateArguments checkUnknown argsDef rawArgs = do
  args <- ordTraverse resolve rawArgs
  ordTraverse_ checkUnknown args
    *> ordTraverse (validateArgument args) (arguments argsDef)

class Resolve f s ctx where
  resolve :: f s -> Validator schemaS ctx (f CONST)

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
