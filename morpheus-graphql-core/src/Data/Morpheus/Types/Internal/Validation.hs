{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation
  ( Validator,
    SelectionValidator,
    InputValidator,
    BaseValidator,
    InputSource (..),
    OperationContext (..),
    runValidator,
    askType,
    askTypeMember,
    selectRequired,
    selectKnown,
    Constraint (..),
    constraint,
    asksScope,
    selectWithDefaultValue,
    startInput,
    inField,
    inputMessagePrefix,
    checkUnused,
    Prop (..),
    constraintInputUnion,
    ScopeKind (..),
    setDirective,
    inputValueSource,
    askVariables,
    Scope (..),
    MissingRequired (..),
    InputContext,
    Unknown,
    askFragments,
    getOperationType,
    selectType,
    FragmentValidator,
    askInterfaceTypes,
    askTypeDefinitions,
    withScope,
    setPosition,
    setSelection,
    ValidatorContext (..),
  )
where

-- Resolution,

import Control.Monad.Except (throwError)
import Data.Morpheus.Internal.Utils
  ( IsMap,
    KeyOf (..),
    member,
    selectBy,
    selectOr,
    throwErrors,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    IN,
    Position (..),
    Ref (..),
    TRUE,
    TypeName,
    Value (..),
    constraintInputUnion,
    fromAny,
    isNullable,
    msg,
    withPath,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
import Data.Morpheus.Types.Internal.Validation.Error
  ( KindViolation (..),
    MissingRequired (..),
    Unknown (..),
    Unused (..),
  )
import Data.Morpheus.Types.Internal.Validation.Internal
  ( askInterfaceTypes,
    askType,
    askTypeMember,
    getOperationType,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
import Relude hiding (Constraint)

getUnused :: (KeyOf k b, IsMap k c, Foldable t) => c a -> t b -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf) . toList

failOnUnused :: (Unused a) => [a] -> Validator s (OperationContext s1 s2) ()
failOnUnused [] = pure ()
failOnUnused (x : xs) = do
  ctx <- Validator ask
  throwErrors $ (`withPath` path (scope ctx)) . unused (localContext ctx) <$> (x :| xs)

checkUnused ::
  ( KeyOf k b,
    IsMap k c,
    Unused b,
    Foldable t
  ) =>
  c a ->
  t b ->
  Validator s (OperationContext s1 s2) ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  (KindViolation k inp) =>
  Constraint (k :: TypeCategory) ->
  inp ->
  TypeDefinition ANY s ->
  Validator s ctx (TypeDefinition k s)
constraint IMPLEMENTABLE _ TypeDefinition {typeContent = DataObject {objectFields, ..}, ..} =
  pure TypeDefinition {typeContent = DataObject {objectFields, ..}, ..}
constraint IMPLEMENTABLE _ TypeDefinition {typeContent = DataInterface fields, ..} =
  pure TypeDefinition {typeContent = DataInterface fields, ..}
constraint INPUT ctx x = maybe (throwError (kindViolation INPUT ctx)) pure (fromAny x)
constraint target ctx _ = throwError (kindViolation target ctx)

selectRequired ::
  ( IsMap FieldName c,
    MissingRequired (c a) ctx
  ) =>
  Ref FieldName ->
  c a ->
  Validator s ctx a
selectRequired selector container =
  do
    ValidatorContext {scope, localContext} <- Validator ask
    selectBy
      (missingRequired scope localContext selector container)
      (keyOf selector)
      container

selectWithDefaultValue ::
  forall ctx c s validValue a.
  ( IsMap FieldName c,
    MissingRequired (c a) ctx
  ) =>
  (Value s -> Validator s ctx validValue) ->
  (a -> Validator s ctx validValue) ->
  FieldDefinition IN s ->
  c a ->
  Validator s ctx validValue
selectWithDefaultValue
  f
  validateF
  field@FieldDefinition
    { fieldName,
      fieldContent
    }
  values =
    selectOr
      (handleNull fieldContent)
      validateF
      fieldName
      values
    where
      ------------------
      handleNull ::
        Maybe (FieldContent TRUE IN s) ->
        Validator s ctx validValue
      handleNull (Just (DefaultInputValue value)) = f value
      handleNull Nothing
        | isNullable field = f Null
        | otherwise = failSelection
      -----------------
      failSelection = do
        ValidatorContext {scope, localContext} <- Validator ask
        position <- asksScope position
        throwError $ missingRequired scope localContext (Ref fieldName (fromMaybe (Position 0 0) position)) values

selectType ::
  TypeName ->
  Validator s ctx (TypeDefinition ANY s)
selectType name =
  asks schema >>= maybe (throwError err) pure . lookupDataType name
  where
    err = "Unknown Type " <> msg name <> "."

selectKnown ::
  ( IsMap k c,
    Unknown sel ctx,
    KeyOf k sel
  ) =>
  sel ->
  c a ->
  Validator s ctx a
selectKnown selector lib =
  do
    ValidatorContext {scope, localContext} <- Validator ask
    selectBy
      (unknown scope localContext selector)
      (keyOf selector)
      lib
