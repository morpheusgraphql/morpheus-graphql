{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
  )
where

import Control.Monad.Trans.Reader
  ( ReaderT (..),
    ask,
    withReaderT,
  )
-- MORPHEUS

import Data.Morpheus.Types.Internal.AST
  ( FieldDefinition (..),
    FieldsDefinition (..),
    Fragments,
    Message,
    Name,
    Object,
    Position,
    Ref (..),
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    Value (..),
    __inputname,
    entryValue,
    isFieldNullable,
    isInputDataType,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
    KeyOf (..),
    Selectable,
    member,
    selectBy,
    selectOr,
    size,
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

getUnused :: (KeyOf b, Selectable ca a) => ca -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused b => [b] -> Validator ctx ()
failOnUnused x
  | null x = return ()
  | otherwise = do
    (gctx, _) <- Validator ask
    failure $ map (unused gctx) x

checkUnused :: (KeyOf b, Selectable ca a, Unused b) => ca -> [b] -> Validator ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  forall (a :: Target) inp ctx.
  KindViolation a inp =>
  Constraint (a :: Target) ->
  inp ->
  TypeDefinition ->
  Validator ctx (Resolution a)
constraint OBJECT _ TypeDefinition {typeContent = DataObject {objectFields}, typeName} =
  pure (typeName, objectFields)
constraint INPUT _ x | isInputDataType x = pure x
constraint target ctx _ = failure [kindViolation target ctx]

selectRequired ::
  ( Selectable c value,
    MissingRequired c ctx
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
    MissingRequired values ctx
  ) =>
  value ->
  FieldDefinition ->
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
    KeyOf (UnknownSelector c)
  ) =>
  UnknownSelector c ->
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
  FieldDefinition ->
  SelectionValidator TypeDefinition
askFieldType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  do
    schema <- askSchema
    selectBy
      [internalError field]
      typeConName
      schema

askTypeMember ::
  Name ->
  SelectionValidator (Name, FieldsDefinition)
askTypeMember name =
  askSchema
    >>= selectOr notFound pure name
    >>= constraintOBJECT
  where
    notFound = do
      scopeType <- askScopeTypeName
      failure $
        "Type \"" <> name
          <> "\" referenced by union \""
          <> scopeType
          <> "\" can't found in Schema."
    --------------------------------------
    constraintOBJECT TypeDefinition {typeName, typeContent} = con typeContent
      where
        con DataObject {objectFields} = pure (typeName, objectFields)
        con _ = do
          scopeType <- askScopeTypeName
          failure $
            "Type \"" <> typeName
              <> "\" referenced by union \""
              <> scopeType
              <> "\" must be an OBJECT."

askInputFieldType ::
  FieldDefinition ->
  InputValidator TypeDefinition
askInputFieldType field@FieldDefinition {fieldName, fieldType = TypeRef {typeConName}} =
  askSchema
    >>= selectBy
      [internalError field]
      typeConName
    >>= constraintINPUT
  where
    constraintINPUT x
      | isInputDataType x = pure x
      | otherwise =
        failure $
          "Type \"" <> typeName x
            <> "\" referenced by field \""
            <> fieldName
            <> "\" must be an input type."

askInputMember ::
  Name ->
  InputValidator TypeDefinition
askInputMember name =
  askSchema
    >>= selectOr notFound pure name
    >>= constraintINPUT_OBJECT
  where
    typeInfo tName =
      "Type \"" <> tName <> "\" referenced by inputUnion "
    notFound = do
      scopeType <- askScopeTypeName
      failure $ typeInfo name <> scopeType <> "\" can't found in Schema."
    --------------------------------------
    constraintINPUT_OBJECT tyDef@TypeDefinition {typeName, typeContent} = con typeContent
      where
        con DataInputObject {} = pure tyDef
        con _ = do
          scopeType <- askScopeTypeName
          failure $ typeInfo typeName <> "\"" <> scopeType <> "\" must be an INPUT_OBJECT."

startInput :: InputSource -> InputValidator a -> Validator ctx a
startInput inputSource =
  setContext $
    const
      InputContext
        { inputSource,
          inputPath = []
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

askScopeTypeName :: Validator ctx Name
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

withScope :: Name -> Ref -> Validator ctx a -> Validator ctx a
withScope scopeTypeName (Ref scopeSelectionName scopePosition) = setGlobalContext update
  where
    update ctx = ctx {scopeTypeName, scopePosition, scopeSelectionName}

withScopePosition :: Position -> Validator ctx a -> Validator ctx a
withScopePosition scopePosition = setGlobalContext update
  where
    update ctx = ctx {scopePosition}

withScopeType :: Name -> Validator ctx a -> Validator ctx a
withScopeType scopeTypeName = setGlobalContext update
  where
    update ctx = ctx {scopeTypeName}

inputMessagePrefix :: InputValidator Message
inputMessagePrefix = renderInputPrefix <$> askContext

constraintInputUnion ::
  forall stage.
  [(Name, Bool)] ->
  Object stage ->
  Either Message (Name, Maybe (Value stage))
constraintInputUnion tags hm = do
  (enum :: Value stage) <-
    entryValue
      <$> selectBy
        ("valid input union should contain \"" <> __inputname <> "\" and actual value")
        __inputname
        hm
  tyName <- isPosibeInputUnion tags enum
  case size hm of
    1 -> pure (tyName, Nothing)
    2 -> do
      value <-
        entryValue
          <$> selectBy
            ("value for Union \"" <> tyName <> "\" was not Provided.")
            tyName
            hm
      pure (tyName, Just value)
    _ -> failure ("input union can have only one variant." :: Message)

isPosibeInputUnion :: [(Name, Bool)] -> Value stage -> Either Message Name
isPosibeInputUnion tags (Enum name) = case lookup name tags of
  Nothing -> failure (name <> " is not posible union type" :: Message)
  _ -> pure name
isPosibeInputUnion _ _ = failure $ "\"" <> __inputname <> "\" must be Enum"
