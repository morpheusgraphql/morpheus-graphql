{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Types.Internal.Validation
  ( Validator,
    SelectionValidator,
    InputValidator,
    BaseValidator,
    InputSource (..),
    Context (..),
    SelectionContext (..),
    runValidator,
    askSchema,
    askContext,
    askFragments,
    askFieldType,
    askTypeMember,
    selectRequired,
    selectKnown,
    Constraint (..),
    constraint,
    withScope,
    withScopeType,
    withScopePosition,
    askScopeTypeName,
    selectWithDefaultValue,
    askScopePosition,
    askInputFieldType,
    askInputMember,
    startInput,
    withInputScope,
    inputMessagePrefix,
    checkUnused,
    Prop (..),
    constraintInputUnion,
    ScopeKind (..),
    withDirective,
  )
where

import Control.Monad.Trans.Reader
  ( ReaderT (..),
    ask,
    withReaderT,
  )
-- MORPHEUS

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
    Directive (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    Fragments,
    IN,
    Message,
    OUT,
    Object,
    Position,
    Ref (..),
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    Value (..),
    __inputname,
    entryValue,
    fromAny,
    isFieldNullable,
    msg,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Morpheus.Types.Internal.Validation.Error
  ( InternalError (..),
    KindViolation (..),
    MissingRequired (..),
    Unknown (..),
    Unused (..),
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( BaseValidator,
    Constraint (..),
    Context (..),
    InputContext (..),
    InputSource (..),
    InputValidator,
    Prop (..),
    Resolution,
    ScopeKind (..),
    SelectionContext (..),
    SelectionValidator,
    Target (..),
    Validator (..),
    renderInputPrefix,
  )
import Data.Semigroup
  ( (<>),
    Semigroup (..),
  )

getUnused :: (KeyOf b, KEY a ~ KEY b, Selectable ca a) => ca -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused b => [b] -> Validator ctx ()
failOnUnused x
  | null x = return ()
  | otherwise = do
    (gctx, _) <- Validator ask
    failure $ map (unused gctx) x

checkUnused :: (KeyOf b, KEY a ~ KEY b, Selectable ca a, Unused b) => ca -> [b] -> Validator ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  forall (a :: Target) inp ctx.
  KindViolation a inp =>
  Constraint (a :: Target) ->
  inp ->
  TypeDefinition ANY ->
  Validator ctx (Resolution a)
constraint OBJECT _ TypeDefinition {typeContent = DataObject {objectFields}, typeName} =
  pure (typeName, objectFields)
constraint INPUT ctx x = maybe (failure [kindViolation INPUT ctx]) pure (fromAny x)
constraint target ctx _ = failure [kindViolation target ctx]

selectRequired ::
  ( Selectable c value,
    MissingRequired c ctx,
    KEY Ref ~ KEY value
  ) =>
  Ref ->
  c ->
  Validator ctx value
selectRequired selector container =
  do
    (gctx, ctx) <- Validator ask
    selectBy
      [missingRequired gctx ctx selector container]
      (keyOf selector)
      container

selectWithDefaultValue ::
  ( Selectable values value,
    MissingRequired values ctx,
    KEY value ~ FieldName
  ) =>
  value ->
  FieldDefinition IN ->
  values ->
  Validator ctx value
selectWithDefaultValue
  fallbackValue
  field@FieldDefinition {fieldName}
  values =
    selectOr
      handleNullable
      pure
      fieldName
      values
    where
      ------------------
      handleNullable
        | isFieldNullable field = pure fallbackValue
        | otherwise = failSelection
      -----------------
      failSelection = do
        (gctx, ctx) <- Validator ask
        failure [missingRequired gctx ctx (Ref fieldName (scopePosition gctx)) values]

selectKnown ::
  ( Selectable c a,
    Unknown c ctx,
    KeyOf sel,
    sel ~ UnknownSelector c,
    KEY sel ~ KEY a
  ) =>
  sel ->
  c ->
  Validator ctx a
selectKnown selector lib =
  do
    (gctx, ctx) <- Validator ask
    selectBy
      (unknown gctx ctx lib selector)
      (keyOf selector)
      lib

askFieldType ::
  FieldDefinition OUT ->
  SelectionValidator (TypeDefinition OUT)
askFieldType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  do
    schema <- askSchema
    anyType <-
      selectBy
        [internalError field]
        typeConName
        schema
    case fromAny anyType of
      Just x -> pure x
      Nothing ->
        failure $
          "Type \"" <> msg (typeName anyType)
            <> "\" referenced by OBJECT \""
            <> "\" must be an OUTPUT_TYPE."

askTypeMember ::
  TypeName ->
  SelectionValidator (TypeName, FieldsDefinition OUT)
askTypeMember name =
  askSchema
    >>= selectOr notFound pure name
    >>= constraintOBJECT
  where
    notFound = do
      scopeType <- askScopeTypeName
      failure $
        "Type \""
          <> msg name
          <> "\" referenced by union \""
          <> msg scopeType
          <> "\" can't found in Schema."
    --------------------------------------
    constraintOBJECT :: TypeDefinition ANY -> SelectionValidator (TypeName, FieldsDefinition OUT)
    constraintOBJECT TypeDefinition {typeName, typeContent} = con typeContent
      where
        con DataObject {objectFields} = pure (typeName, objectFields)
        con _ = do
          scopeType <- askScopeTypeName
          failure $
            "Type \"" <> msg typeName
              <> "\" referenced by union \""
              <> msg scopeType
              <> "\" must be an OBJECT."

askInputFieldType ::
  FieldDefinition IN ->
  InputValidator (TypeDefinition IN)
askInputFieldType field@FieldDefinition {fieldName, fieldType = TypeRef {typeConName}} =
  askSchema
    >>= selectBy
      [internalError field]
      typeConName
    >>= constraintINPUT
  where
    constraintINPUT :: TypeDefinition ANY -> InputValidator (TypeDefinition IN)
    constraintINPUT x = case (fromAny x :: Maybe (TypeDefinition IN)) of
      Just inputType -> pure inputType
      Nothing ->
        failure $
          "Type \""
            <> msg (typeName x)
            <> "\" referenced by field \""
            <> msg fieldName
            <> "\" must be an input type."

askInputMember ::
  TypeName ->
  InputValidator (TypeDefinition IN)
askInputMember name =
  askSchema
    >>= selectOr notFound pure name
    >>= constraintINPUT_OBJECT
  where
    typeInfo tName =
      "Type \"" <> msg tName <> "\" referenced by inputUnion "
    notFound = do
      scopeType <- askScopeTypeName
      failure $
        typeInfo name
          <> msg scopeType
          <> "\" can't found in Schema."
    --------------------------------------
    constraintINPUT_OBJECT :: TypeDefinition ANY -> InputValidator (TypeDefinition IN)
    constraintINPUT_OBJECT TypeDefinition {typeContent, ..} = con (fromAny typeContent)
      where
        con :: Maybe (TypeContent a IN) -> InputValidator (TypeDefinition IN)
        con (Just content@DataInputObject {}) = pure TypeDefinition {typeContent = content, ..}
        con _ = do
          scopeType <- askScopeTypeName
          failure $
            typeInfo typeName
              <> "\""
              <> msg scopeType
              <> "\" must be an INPUT_OBJECT."

startInput :: InputSource -> InputValidator a -> Validator ctx a
startInput inputSource =
  setContext $
    const
      InputContext
        { inputSource,
          inputPath = []
        }

withDirective :: Directive s -> Validator ctx a -> Validator ctx a
withDirective
  Directive
    { directiveName,
      directivePosition
    } = setGlobalContext update
    where
      update ctx =
        ctx
          { scopePosition = directivePosition,
            scopeSelectionName = directiveName,
            scopeKind = DIRECTIVE
          }

withInputScope :: Prop -> InputValidator a -> InputValidator a
withInputScope prop = setContext update
  where
    update ctx@InputContext {inputPath = old} =
      ctx {inputPath = old <> [prop]}

runValidator :: Validator ctx a -> Context -> ctx -> Eventless a
runValidator (Validator x) globalCTX ctx = runReaderT x (globalCTX, ctx)

askContext :: Validator ctx ctx
askContext = snd <$> Validator ask

askSchema :: Validator ctx Schema
askSchema = schema . fst <$> Validator ask

askFragments :: Validator ctx Fragments
askFragments = fragments . fst <$> Validator ask

askScopeTypeName :: Validator ctx TypeName
askScopeTypeName = scopeTypeName . fst <$> Validator ask

askScopePosition :: Validator ctx Position
askScopePosition = scopePosition . fst <$> Validator ask

setContext ::
  (c' -> c) ->
  Validator c a ->
  Validator c' a
setContext f = Validator . withReaderT (\(x, y) -> (x, f y)) . _runValidator

setGlobalContext ::
  (Context -> Context) ->
  Validator c a ->
  Validator c a
setGlobalContext f = Validator . withReaderT (\(x, y) -> (f x, y)) . _runValidator

withScope :: TypeName -> Ref -> Validator ctx a -> Validator ctx a
withScope scopeTypeName (Ref scopeSelectionName scopePosition) = setGlobalContext update
  where
    update ctx = ctx {scopeTypeName, scopePosition, scopeSelectionName}

withScopePosition :: Position -> Validator ctx a -> Validator ctx a
withScopePosition scopePosition = setGlobalContext update
  where
    update ctx = ctx {scopePosition}

withScopeType :: TypeName -> Validator ctx a -> Validator ctx a
withScopeType scopeTypeName = setGlobalContext update
  where
    update ctx = ctx {scopeTypeName}

inputMessagePrefix :: InputValidator Message
inputMessagePrefix = renderInputPrefix <$> askContext

constraintInputUnion ::
  forall stage.
  [(TypeName, Bool)] ->
  Object stage ->
  Either Message (TypeName, Maybe (Value stage))
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
  tyName <- isPosibeInputUnion tags enum
  case size hm of
    1 -> pure (tyName, Nothing)
    2 -> do
      value <-
        entryValue
          <$> selectBy
            ( "value for Union \""
                <> msg tyName
                <> "\" was not Provided."
            )
            (toFieldName tyName)
            hm
      pure (tyName, Just value)
    _ -> failure ("input union can have only one variant." :: Message)

isPosibeInputUnion :: [(TypeName, Bool)] -> Value stage -> Either Message TypeName
isPosibeInputUnion tags (Enum name) = case lookup name tags of
  Nothing ->
    failure
      ( msg name
          <> " is not posible union type"
      )
  _ -> pure name
isPosibeInputUnion _ _ =
  failure $
    "\""
      <> msg __inputname
      <> "\" must be Enum"
