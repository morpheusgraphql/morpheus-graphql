{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    askTypeByRef,
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
  )
where

-- MORPHEUS

import Control.Applicative (pure)
import Control.Monad.Trans.Reader
  ( ask,
  )
import Data.Either (Either)
import Data.Foldable (null)
import Data.Functor ((<$>), fmap)
import Data.List (filter)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
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
    UnionMember (..),
    Value (..),
    __inputname,
    entryValue,
    fromAny,
    isNullable,
    msg,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Validation.Error
  ( KindViolation (..),
    MissingRequired (..),
    Unknown (..),
    Unused (..),
  )
import Data.Morpheus.Types.Internal.Validation.Internal
  ( askTypeByRef,
    askTypeMember,
    getOperationType,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( BaseValidator,
    Constraint (..),
    CurrentSelection (..),
    DirectiveValidator,
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
import Data.Semigroup
  ( (<>),
  )
import Prelude
  ( ($),
    (.),
    not,
    otherwise,
  )

getUnused :: (KeyOf b, KEY a ~ KEY b, Selectable a c) => c -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused ctx b => [b] -> Validator ctx ()
failOnUnused x
  | null x = pure ()
  | otherwise = do
    ctx <- unValidatorContext <$> Validator ask
    failure $ fmap (unused ctx) x

checkUnused :: (KeyOf b, KEY a ~ KEY b, Selectable a ca, Unused ctx b) => ca -> [b] -> Validator ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  forall (a :: Target) inp ctx s.
  KindViolation a inp =>
  Constraint (a :: Target) ->
  inp ->
  TypeDefinition ANY s ->
  Validator ctx (Resolution s a)
constraint OBJECT _ TypeDefinition {typeContent = DataObject {objectFields}, typeName} =
  pure (typeName, objectFields)
constraint INPUT ctx x = maybe (failure [kindViolation INPUT ctx]) pure (fromAny x)
constraint target ctx _ = failure [kindViolation target ctx]

selectRequired ::
  ( Selectable value c,
    MissingRequired c ctx,
    KEY Ref ~ KEY value
  ) =>
  Ref ->
  c ->
  Validator ctx value
selectRequired selector container =
  do
    ValidatorContext scope ctx <- Validator ask
    selectBy
      [missingRequired scope ctx selector container]
      (keyOf selector)
      container

selectWithDefaultValue ::
  forall ctx values value s validValue.
  ( Selectable value values,
    MissingRequired values ctx,
    KEY value ~ FieldName,
    MonadContext Validator ctx
  ) =>
  (Value s -> Validator ctx validValue) ->
  (value -> Validator ctx validValue) ->
  FieldDefinition IN s ->
  values ->
  Validator ctx validValue
selectWithDefaultValue
  f
  validateF
  field@FieldDefinition
    { fieldName,
      fieldContent
    }
  values =
    selectOr
      (handeNull fieldContent)
      validateF
      fieldName
      values
    where
      ------------------
      handeNull ::
        Maybe (FieldContent TRUE IN s) ->
        Validator ctx validValue
      handeNull (Just (DefaultInputValue value)) = f value
      handeNull Nothing
        | isNullable field = f Null
        | otherwise = failSelection
      -----------------
      failSelection = do
        ValidatorContext scope ctx <- Validator ask
        position <- asksScope position
        failure [missingRequired scope ctx (Ref fieldName (fromMaybe (Position 0 0) position)) values]

selectKnown ::
  ( Selectable a c,
    Unknown c sel ctx,
    KeyOf sel,
    KEY sel ~ KEY a
  ) =>
  sel ->
  c ->
  Validator ctx a
selectKnown selector lib =
  do
    ValidatorContext scope ctx <- Validator ask
    selectBy
      (unknown scope ctx lib selector)
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
  unionMember <- isPosibeInputUnion tags enum
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

isPosibeInputUnion :: [UnionMember IN s] -> Value stage -> Either Message (UnionMember IN s)
isPosibeInputUnion tags (Enum name) =
  selectBy
    (msg name <> " is not posible union type")
    name
    tags
isPosibeInputUnion _ _ = failure $ "\"" <> msg __inputname <> "\" must be Enum"
