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
    DirectiveValidator,
    askType,
    askTypeMember,
    selectRequired,
    selectKnown,
    Constraint (..),
    constraint,
    withScope,
    withScopeType,
    withPosition,
    asks,
    asksScope,
    selectWithDefaultValue,
    startInput,
    inField,
    inputMessagePrefix,
    checkUnused,
    Prop (..),
    constraintInputUnion,
    ScopeKind (..),
    withDirective,
    inputValueSource,
    askVariables,
    Scope (..),
    MissingRequired (..),
    InputContext,
    GetWith,
    SetWith,
    Unknown,
    askSchema,
    askFragments,
    MonadContext,
    CurrentSelection (..),
    getOperationType,
    selectType,
    FragmentValidator,
    askInterfaceTypes,
    validateOptional,
    askTypeDefinitions,
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
    throwMany,
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
    msgValidation,
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
import Relude hiding
  ( Constraint,
    asks,
  )

validateOptional :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
validateOptional = traverse

getUnused :: (KeyOf k b, IsMap k c, Foldable t) => c a -> t b -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf) . toList

failOnUnused :: Unused ctx b => [b] -> Validator s ctx ()
failOnUnused [] = pure ()
failOnUnused (x : xs) = do
  ctx <- validatorCTX <$> Validator ask
  throwMany $ unused ctx <$> (x :| xs)

checkUnused ::
  ( KeyOf k b,
    IsMap k c,
    Unused ctx b,
    Foldable t
  ) =>
  c a ->
  t b ->
  Validator s ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  KindViolation k inp =>
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
    ValidatorContext {scope, validatorCTX} <- Validator ask
    selectBy
      (missingRequired scope validatorCTX selector container)
      (keyOf selector)
      container

selectWithDefaultValue ::
  forall ctx c s validValue a.
  ( IsMap FieldName c,
    MissingRequired (c a) ctx,
    MonadContext (Validator s) s ctx
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
        ValidatorContext {scope, validatorCTX} <- Validator ask
        position <- asksScope position
        throwError $ missingRequired scope validatorCTX (Ref fieldName (fromMaybe (Position 0 0) position)) values

selectType ::
  TypeName ->
  Validator s ctx (TypeDefinition ANY s)
selectType name =
  askSchema >>= maybe (throwError err) pure . lookupDataType name
  where
    err = "Unknown Type " <> msgValidation name <> "."

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
    ValidatorContext {scope, validatorCTX} <- Validator ask
    selectBy
      (unknown scope validatorCTX selector)
      (keyOf selector)
      lib
