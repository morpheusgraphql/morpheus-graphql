{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
  )
where

import Data.Morpheus.Internal.Utils
  ( Failure (..),
    KeyOf (..),
    Selectable,
    member,
    selectBy,
    selectOr,
    size,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    IN,
    Message,
    Object,
    ObjectEntry (..),
    Position (..),
    Ref (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    UnionMember (..),
    ValidationError,
    Value (..),
    __inputname,
    entryValue,
    fromAny,
    isNullable,
    msg,
    msgValidation,
    toFieldName,
  )
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
  ( BaseValidator,
    Constraint (..),
    CurrentSelection (..),
    DirectiveValidator,
    FragmentValidator,
    GetWith (..),
    InputContext,
    InputSource (..),
    InputValidator,
    MonadContext (..),
    OperationContext (..),
    Prop (..),
    Resolution,
    Scope (..),
    ScopeKind (..),
    SelectionValidator,
    SetWith (..),
    Target (..),
    Validator (..),
    ValidatorContext (..),
    askFragments,
    askSchema,
    askVariables,
    asks,
    asksScope,
    inField,
    inputMessagePrefix,
    inputValueSource,
    runValidator,
    startInput,
    withDirective,
    withPosition,
    withScope,
    withScopeType,
  )
import Relude hiding
  ( Constraint,
    asks,
  )

getUnused :: (KeyOf k b, Selectable k a c) => c -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused ctx b => [b] -> Validator s ctx ()
failOnUnused x
  | null x = pure ()
  | otherwise = do
    ctx <- validatorCTX <$> Validator ask
    failure $ fmap (unused ctx) x

checkUnused ::
  ( KeyOf k b,
    Selectable k a ca,
    Unused ctx b
  ) =>
  ca ->
  [b] ->
  Validator s ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  KindViolation a inp =>
  Constraint (a :: Target) ->
  inp ->
  TypeDefinition ANY s ->
  Validator s ctx (Resolution s a)
constraint IMPLEMENTABLE _ TypeDefinition {typeContent = DataObject {objectFields, ..}, ..} =
  pure TypeDefinition {typeContent = DataObject {objectFields, ..}, ..}
constraint IMPLEMENTABLE _ TypeDefinition {typeContent = DataInterface fields, ..} =
  pure TypeDefinition {typeContent = DataInterface fields, ..}
constraint INPUT ctx x = maybe (failure [kindViolation INPUT ctx]) pure (fromAny x)
constraint target ctx _ = failure [kindViolation target ctx]

selectRequired ::
  ( Selectable FieldName value c,
    MissingRequired c ctx
  ) =>
  Ref ->
  c ->
  Validator s ctx value
selectRequired selector container =
  do
    ValidatorContext {scope, validatorCTX} <- Validator ask
    selectBy
      [missingRequired scope validatorCTX selector container]
      (keyOf selector)
      container

selectWithDefaultValue ::
  forall ctx values value s validValue.
  ( Selectable FieldName value values,
    MissingRequired values ctx,
    MonadContext (Validator s) s ctx
  ) =>
  (Value s -> Validator s ctx validValue) ->
  (value -> Validator s ctx validValue) ->
  FieldDefinition IN s ->
  values ->
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
        failure [missingRequired scope validatorCTX (Ref fieldName (fromMaybe (Position 0 0) position)) values]

selectType ::
  TypeName ->
  Validator s ctx (TypeDefinition ANY s)
selectType name =
  askSchema >>= selectBy err name
  where
    err = "Unknown Type " <> msgValidation name <> "." :: ValidationError

selectKnown ::
  ( Selectable k a c,
    Unknown c sel ctx,
    KeyOf k sel
  ) =>
  sel ->
  c ->
  Validator s ctx a
selectKnown selector lib =
  do
    ValidatorContext {scope, validatorCTX} <- Validator ask
    selectBy
      (unknown scope validatorCTX lib selector)
      (keyOf selector)
      lib

constraintInputUnion ::
  forall stage schemaStage.
  [UnionMember IN schemaStage] ->
  Object stage ->
  Either Message (UnionMember IN schemaStage, Maybe (Value stage))
constraintInputUnion tags hm = do
  (enum :: Value stage) <-
    entryValue
      <$> selectBy
        ( "valid input union should contain \""
            <> msg __inputname
            <> "\" and actual value"
        )
        __inputname
        hm
  unionMember <- isPossibleInputUnion tags enum
  case size hm of
    1 -> pure (unionMember, Nothing)
    2 -> do
      value <-
        entryValue
          <$> selectBy
            ( "value for Union \""
                <> msg unionMember
                <> "\" was not Provided."
            )
            (toFieldName $ memberName unionMember)
            hm
      pure (unionMember, Just value)
    _ -> failure ("input union can have only one variant." :: Message)

isPossibleInputUnion :: [UnionMember IN s] -> Value stage -> Either Message (UnionMember IN s)
isPossibleInputUnion tags (Enum name) =
  selectBy
    (msg name <> " is not possible union type")
    name
    tags
isPossibleInputUnion _ _ = failure $ "\"" <> msg __inputname <> "\" must be Enum"
